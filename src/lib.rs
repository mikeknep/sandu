use itertools::Itertools;
use serde::Deserialize;
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fmt;
use std::io;
use structopt::StructOpt;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::{
    backend::{Backend, TermionBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Style},
    text::Span,
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph},
    Terminal,
};

#[derive(StructOpt)]
#[structopt(name = "sandu", about = "Interactive Terraform state surgery")]
pub struct Sandu {
    planfile: String,
}

pub struct Clients<'a> {
    pub filesystem: &'a dyn Filesystem,
    pub terraform: &'a dyn Terraform,
}

#[derive(Debug)]
pub struct SanduError {
    details: String,
}

impl SanduError {
    pub fn new(msg: &str) -> Box<SanduError> {
        Box::new(SanduError {
            details: msg.to_string(),
        })
    }
}

impl fmt::Display for SanduError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for SanduError {
    fn description(&self) -> &str {
        &self.details
    }
}

pub fn run(sandu: Sandu, clients: Clients) -> Result<(), Box<dyn Error>> {
    if !clients.filesystem.file_exists(&sandu.planfile) {
        return Err(SanduError::new("Provided file does not exist"));
    }
    let json_bytes = clients.terraform.show_plan(&sandu.planfile)?;
    let tfplan = serde_json::from_slice::<TfPlan>(&json_bytes)?;

    let mut mdl = Mdl::new();
    let mut model = Model::init(&tfplan);

    let stdout = io::stdout().into_raw_mode()?;
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let mut keys = io::stdin().keys();
    terminal.clear()?;

    loop {
        draw(&mut terminal, &mut model)?;
        let key = keys
            .next()
            .ok_or_else(|| SanduError::new("Error during keypress event"))??;
        if let Some(new_state) = handle_keypress(&tfplan, mdl.state, key) {
            mdl.state = new_state;
        } else {
            break;
        }
    }
    Ok(())
}

fn draw<B>(terminal: &mut Terminal<B>, model: &mut Model) -> Result<(), Box<dyn Error>>
where
    B: Backend,
{
    terminal.draw(|f| {
        let panes = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(30), Constraint::Percentage(70)].as_ref())
            .split(f.size());

        let operations_pane = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Percentage(20),
                    Constraint::Percentage(40),
                    Constraint::Percentage(40),
                ]
                .as_ref(),
            )
            .split(panes[0]);

        let preview_pane = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Percentage(70),
                    Constraint::Percentage(20),
                    Constraint::Percentage(10),
                ]
                .as_ref(),
            )
            .split(panes[1]);

        let resource_preview = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
            .split(preview_pane[0]);

        let types_list_values: Vec<ListItem> = model
            .types()
            .iter()
            .map(|t| ListItem::new(Span::raw(t.clone())))
            .collect();

        let list = List::new(types_list_values)
            .block(Pane::TypesList.draw(&model.active_pane))
            .highlight_style(Style::default().bg(Color::Green));

        f.render_stateful_widget(list, operations_pane[0], &mut model.types_list_state);

        let destroying_list_values: Vec<ListItem> = if let Some(r#type) = model.selected_type() {
            model
                .planned_deletions
                .iter()
                .filter(|resource| r#type == resource.r#type)
                .map(|resource| ListItem::new(Span::raw(resource.address.clone())))
                .collect()
        } else {
            Vec::new()
        };

        let destroying_list = List::new(destroying_list_values)
            .block(Pane::DestroyingList.draw(&model.active_pane))
            .highlight_style(Style::default().bg(Color::Green));

        f.render_stateful_widget(
            destroying_list,
            operations_pane[1],
            &mut model.destroying_list_state,
        );

        let creating_list_values: Vec<ListItem> = if let Some(r#type) = model.selected_type() {
            model
                .planned_creations
                .iter()
                .filter(|resource| r#type == resource.r#type)
                .map(|resource| ListItem::new(Span::raw(resource.address.clone())))
                .collect()
        } else {
            Vec::new()
        };

        let creating_list = List::new(creating_list_values)
            .block(Pane::CreatingList.draw(&model.active_pane))
            .highlight_style(Style::default().bg(Color::Green));

        f.render_stateful_widget(
            creating_list,
            operations_pane[2],
            &mut model.creating_list_state,
        );

        let pending_deletion_preview =
            if let Some(resource) = model.selected_resource_pending_deletion() {
                serde_json::to_string_pretty(&resource.preview).unwrap()
            } else {
                "".to_string()
            };
        f.render_widget(
            Paragraph::new(pending_deletion_preview)
                .block(Pane::DestroyingPreview.draw(&model.active_pane)),
            resource_preview[0],
        );

        let pending_creation_preview =
            if let Some(resource) = model.selected_resource_pending_creation() {
                serde_json::to_string_pretty(&resource.preview).unwrap()
            } else {
                "".to_string()
            };
        f.render_widget(
            Paragraph::new(pending_creation_preview)
                .block(Pane::CreatingPreview.draw(&model.active_pane)),
            resource_preview[1],
        );

        f.render_widget(
            Pane::StagedOperations.draw(&model.active_pane),
            preview_pane[1],
        );
        f.render_widget(Pane::Help.draw(&model.active_pane), preview_pane[2]);
    })?;
    Ok(())
}

#[derive(Deserialize)]
struct TfPlan {
    #[serde(rename(deserialize = "resource_changes"))]
    changing_resources: Vec<ChangingResource>,
}

impl TfPlan {
    fn unique_types(&self) -> Vec<String> {
        self.changing_resources
            .iter()
            .map(|resource| resource.r#type.clone())
            .unique()
            .sorted()
            .collect()
    }
}

#[derive(Deserialize)]
struct ChangingResource {
    address: String,
    change: Change,
    r#type: String,
}

#[derive(Deserialize)]
struct Change {
    actions: Vec<String>,
    after: Option<serde_json::Value>,
    before: Option<serde_json::Value>,
}

#[derive(Debug, PartialEq)]
struct TerraformResource {
    address: String,
    preview: serde_json::Value,
    r#type: String,
}

#[derive(Debug, PartialEq)]
struct TerraformPlan {
    pending_creation: Vec<TerraformResource>,
    pending_deletion: Vec<TerraformResource>,
}

impl From<TfPlan> for TerraformPlan {
    fn from(tfplan: TfPlan) -> Self {
        let mut pending_creation = vec![];
        let mut pending_deletion = vec![];

        for changing_resource in tfplan.changing_resources {
            match (
                changing_resource.change.before,
                changing_resource.change.after,
            ) {
                (Some(before), None) => {
                    pending_deletion.push(TerraformResource {
                        address: changing_resource.address.clone(),
                        r#type: changing_resource.r#type.clone(),
                        preview: before,
                    });
                }
                (None, Some(after)) => {
                    pending_creation.push(TerraformResource {
                        address: changing_resource.address.clone(),
                        r#type: changing_resource.r#type.clone(),
                        preview: after,
                    });
                }
                _ => {}
            }
        }

        TerraformPlan {
            pending_creation,
            pending_deletion,
        }
    }
}

pub trait Terraform {
    fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, Box<dyn Error>>;
}

pub trait Filesystem {
    fn file_exists(&self, path: &str) -> bool;
}

struct Model {
    active_pane: Pane,
    creating_list_state: ListState,
    destroying_list_state: ListState,
    types_list_state: ListState,
    planned_creations: Vec<Resource>,
    planned_deletions: Vec<Resource>,
    staged_operations: Vec<Operation>,
}

#[derive(Debug, PartialEq)]
struct Mdl {
    staged_operations: Vec<Operation>,
    state: State,
}

impl Mdl {
    fn new() -> Self {
        Mdl {
            staged_operations: vec![],
            state: State::ChoosingType(ChoosingType { selected: None }),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum TerraformAction {
    Create,
    Delete,
}

#[derive(Debug, PartialEq)]
struct ChoosingType {
    selected: Option<usize>,
}

#[derive(Debug, PartialEq)]
struct BrowsingResources {
    action_in_scope: TerraformAction,
    r#type: String,
    selected_create: Option<usize>,
    selected_delete: Option<usize>,
}

#[derive(Debug, PartialEq)]
enum State {
    ChoosingType(ChoosingType),
    BrowsingResources(BrowsingResources),
}

fn handle_keypress(tfplan: &TfPlan, state: State, key: Key) -> Option<State> {
    if let Key::Char('q') = key {
        return None;
    }
    match state {
        State::ChoosingType(current) => handle_keypress_while_choosing_type(tfplan, current, key),
        State::BrowsingResources(current) => {
            handle_keypress_while_browsing_resources(tfplan, current, key)
        }
    }
}

fn handle_keypress_while_choosing_type(
    tfplan: &TfPlan,
    state: ChoosingType,
    key: Key,
) -> Option<State> {
    match key {
        Key::Char('j') | Key::Down => {
            let next_index = cycle_next(tfplan.unique_types().len(), &state.selected);
            Some(State::ChoosingType(ChoosingType {
                selected: Some(next_index),
            }))
        }
        Key::Char('k') | Key::Up => {
            let previous_index = cycle_previous(tfplan.unique_types().len(), &state.selected);
            Some(State::ChoosingType(ChoosingType {
                selected: Some(previous_index),
            }))
        }
        Key::Char('\n') => {
            if let Some(i) = state.selected {
                Some(State::BrowsingResources(BrowsingResources {
                    action_in_scope: TerraformAction::Delete,
                    r#type: tfplan.unique_types()[i].clone(),
                    selected_create: None,
                    selected_delete: None,
                }))
            } else {
                Some(State::ChoosingType(ChoosingType {
                    selected: state.selected,
                }))
            }
        }
        _ => Some(State::ChoosingType(ChoosingType {
            selected: state.selected,
        })),
    }
}

fn handle_keypress_while_browsing_resources(
    tfplan: &TfPlan,
    state: BrowsingResources,
    key: Key,
) -> Option<State> {
    match key {
        Key::Char('h') | Key::Left => Some(State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: state.r#type.clone(),
            selected_create: state.selected_create,
            selected_delete: state.selected_delete,
        })),
        Key::Char('l') | Key::Right => Some(State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: state.r#type.clone(),
            selected_create: state.selected_create,
            selected_delete: state.selected_delete,
        })),
        _ => Some(State::BrowsingResources(BrowsingResources {
            action_in_scope: state.action_in_scope.clone(),
            r#type: state.r#type.clone(),
            selected_create: state.selected_create,
            selected_delete: state.selected_delete,
        })),
    }
}

fn cycle_next(items_length: usize, selected: &Option<usize>) -> usize {
    match selected {
        Some(i) => {
            if *i >= items_length - 1 {
                0
            } else {
                i + 1
            }
        }
        None => 0,
    }
}

fn cycle_previous(items_length: usize, selected: &Option<usize>) -> usize {
    match selected {
        Some(i) => {
            if *i == 0 {
                items_length - 1
            } else {
                i - 1
            }
        }
        None => items_length - 1,
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Resource {
    address: String,
    planned_action: PlannedAction,
    preview: serde_json::Value,
    status: Status,
    r#type: String,
}

impl Ord for Resource {
    fn cmp(&self, other: &Self) -> Ordering {
        self.address.cmp(&other.address)
    }
}

impl PartialOrd for Resource {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.address.cmp(&other.address))
    }
}

impl TryFrom<&ChangingResource> for Resource {
    type Error = &'static str;

    fn try_from(cr: &ChangingResource) -> Result<Self, Self::Error> {
        let planned_action = match cr
            .change
            .actions
            .iter()
            .map(|a| a.as_str())
            .collect::<Vec<&str>>()[..]
        {
            ["create"] => PlannedAction::Create,
            ["delete"] => PlannedAction::Delete,
            _ => return Err("No state operations exist to affect planned actions"),
        };

        let preview = match (&cr.change.before, &cr.change.after) {
            (Some(before), None) => before,
            (None, Some(after)) => after,
            _ => return Err("No state operations exist to affect planned actions"),
        };

        Ok(Resource {
            address: cr.address.clone(),
            planned_action,
            preview: preview.clone(),
            r#type: cr.r#type.clone(),
            status: Status::Unstaged,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
enum PlannedAction {
    Create,
    Delete,
}

#[derive(Debug, PartialEq, Eq)]
enum Status {
    Staged,
    Unstaged,
}

#[derive(Debug, PartialEq)]
enum Operation {
    Move { from: String, to: String },
    Import { address: String, identifier: String },
    Remove(String),
}

impl Model {
    fn init(tfplan: &TfPlan) -> Model {
        let creates_and_deletes =
            tfplan
                .changing_resources
                .iter()
                .fold((vec![], vec![]), |mut acc, cr| {
                    let conversion: Result<Resource, _> = cr.try_into();
                    if let Ok(resource) = conversion {
                        match resource.planned_action {
                            PlannedAction::Create => acc.0.push(resource),
                            PlannedAction::Delete => acc.1.push(resource),
                        }
                    }
                    acc
                });

        Model {
            active_pane: Pane::TypesList,
            creating_list_state: ListState::default(),
            destroying_list_state: ListState::default(),
            types_list_state: ListState::default(),
            planned_creations: creates_and_deletes.0,
            planned_deletions: creates_and_deletes.1,
            staged_operations: vec![],
        }
    }

    fn types(&self) -> Vec<String> {
        self.planned_creations
            .iter()
            .chain(self.planned_deletions.iter())
            .map(|resource| resource.r#type.clone())
            .unique()
            .sorted()
            .collect()
    }

    fn unstaged_creations_for_type(&self) -> Vec<&Resource> {
        if let Some(r#type) = self.selected_type() {
            self.planned_creations
                .iter()
                .filter(|resource| resource.r#type == r#type)
                .sorted()
                .collect()
        } else {
            Vec::new()
        }
    }

    fn unstaged_deletions_for_type(&self) -> Vec<&Resource> {
        if let Some(r#type) = self.selected_type() {
            self.planned_deletions
                .iter()
                .filter(|resource| resource.r#type == r#type)
                .sorted()
                .collect()
        } else {
            Vec::new()
        }
    }

    fn selected_type(&self) -> Option<String> {
        self.types_list_state
            .selected()
            .map(|i| self.types()[i].clone())
    }

    fn selected_resource_pending_deletion(&self) -> Option<&Resource> {
        self.destroying_list_state
            .selected()
            .map(|i| self.unstaged_deletions_for_type()[i])
    }

    fn selected_resource_pending_creation(&self) -> Option<&Resource> {
        self.creating_list_state
            .selected()
            .map(|i| self.unstaged_creations_for_type()[i])
    }
}

#[derive(PartialEq)]
enum Pane {
    TypesList,
    DestroyingList,
    CreatingList,
    DestroyingPreview,
    CreatingPreview,
    StagedOperations,
    Help,
}

impl Pane {
    fn draw(&self, active_pane: &Pane) -> Block {
        let text = match *self {
            Pane::TypesList => "Types",
            Pane::DestroyingList | Pane::DestroyingPreview => "Destroying",
            Pane::CreatingList | Pane::CreatingPreview => "Creating",
            Pane::StagedOperations => "Staged",
            Pane::Help => "Help",
        };
        let border_color = if self == active_pane {
            Color::Yellow
        } else {
            Color::Gray
        };

        Block::default()
            .title(vec![Span::from(text)])
            .border_style(Style::default().fg(border_color))
            .borders(Borders::ALL)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use serde_json::json;

    struct FailClient {}

    impl Terraform for FailClient {
        fn show_plan(&self, _: &str) -> Result<Vec<u8>, Box<dyn Error>> {
            Err(SanduError::new("Terraform failed!"))
        }
    }

    impl Filesystem for FailClient {
        fn file_exists(&self, _: &str) -> bool {
            return false;
        }
    }

    #[test]
    fn returns_error_when_planfile_does_not_exist() {
        let sandu = Sandu {
            planfile: "does_not_exist".to_string(),
        };
        let clients = Clients {
            filesystem: &FailClient {},
            terraform: &FailClient {},
        };

        let result = run(sandu, clients);
        assert!(result.is_err());
    }

    #[test]
    fn a_changing_resource_with_two_actions_is_immovable_and_cannot_be_coerced_to_a_model_resource()
    {
        let invalid = ChangingResource {
            address: "module.original.random_pet.tapu".to_string(),
            r#type: "random_pet".to_string(),
            change: Change {
                actions: vec!["create".to_string(), "delete".to_string()],
                before: Some(json!({ "id": "careful-escargot", "separator" : "-" })),
                after: Some(json!({ "separator" : "-" })),
            },
        };
        let failed_convert: Result<Resource, &'static str> = (&invalid).try_into();

        assert!(failed_convert.is_err());
    }

    #[test]
    fn initialize_a_model_from_a_plan() {
        let tfplan = TfPlan {
            changing_resources: vec![
                ChangingResource {
                    address: "module.changed.random_pet.tapu".to_string(),
                    r#type: "random_pet".to_string(),
                    change: Change {
                        actions: vec!["create".to_string()],
                        before: None,
                        after: Some(json!({ "separator" : "-" })),
                    },
                },
                ChangingResource {
                    address: "module.original.random_pet.tapu".to_string(),
                    r#type: "random_pet".to_string(),
                    change: Change {
                        actions: vec!["delete".to_string()],
                        before: Some(json!({ "id": "careful-escargot", "separator" : "-" })),
                        after: None,
                    },
                },
            ],
        };
        let model = Model::init(&tfplan);

        assert_eq!(1, model.planned_creations.len());
        assert_eq!(1, model.planned_deletions.len());

        let expected_create = Resource {
            address: "module.changed.random_pet.tapu".to_string(),
            planned_action: PlannedAction::Create,
            preview: json!({ "separator": "-" }),
            status: Status::Unstaged,
            r#type: "random_pet".to_string(),
        };
        assert_eq!(expected_create, model.planned_creations[0]);

        let expected_delete = Resource {
            address: "module.original.random_pet.tapu".to_string(),
            planned_action: PlannedAction::Delete,
            preview: json!({ "id": "careful-escargot", "separator": "-" }),
            status: Status::Unstaged,
            r#type: "random_pet".to_string(),
        };
        assert_eq!(expected_delete, model.planned_deletions[0]);
    }

    #[test]
    fn lists_unique_types_in_scope() {
        let model = Model {
            active_pane: Pane::TypesList,
            planned_creations: vec![
                Resource {
                    address: "not.relevant".to_string(),
                    planned_action: PlannedAction::Create,
                    preview: json!(""),
                    status: Status::Unstaged,
                    r#type: "random_pet".to_string(),
                },
                Resource {
                    address: "also.irrelevant".to_string(),
                    planned_action: PlannedAction::Create,
                    preview: json!(""),
                    status: Status::Unstaged,
                    r#type: "random_pet".to_string(),
                },
            ],
            planned_deletions: vec![
                Resource {
                    address: "not.relevant".to_string(),
                    planned_action: PlannedAction::Create,
                    preview: json!(""),
                    status: Status::Unstaged,
                    r#type: "random_string".to_string(),
                },
                Resource {
                    address: "not.relevant".to_string(),
                    planned_action: PlannedAction::Create,
                    preview: json!(""),
                    status: Status::Unstaged,
                    r#type: "random_password".to_string(),
                },
            ],
            staged_operations: vec![],
            creating_list_state: ListState::default(),
            destroying_list_state: ListState::default(),
            types_list_state: ListState::default(),
        };

        assert_eq!(
            vec![
                "random_password".to_string(),
                "random_pet".to_string(),
                "random_string".to_string(),
            ],
            model.types()
        );
    }

    #[test]
    fn create_terraform_plan_from_parsed_tfplan() {
        let tfplan = TfPlan {
            changing_resources: vec![
                ChangingResource {
                    address: "example.pending_creation".to_string(),
                    r#type: "example".to_string(),
                    change: Change {
                        actions: vec!["create".to_string()],
                        before: None,
                        after: Some(json!({ "example": "pending_creation" })),
                    },
                },
                ChangingResource {
                    address: "example.pending_deletion".to_string(),
                    r#type: "example".to_string(),
                    change: Change {
                        actions: vec!["delete".to_string()],
                        before: Some(json!({ "example": "pending_deletion" })),
                        after: None,
                    },
                },
                ChangingResource {
                    address: "example.create_and_delete".to_string(),
                    r#type: "example".to_string(),
                    change: Change {
                        actions: vec!["create".to_string(), "delete".to_string()],
                        before: Some(json!({ "example": "create_and_delete" })),
                        after: Some(json!({ "example": "create_and_delete" })),
                    },
                },
            ],
        };

        let expected_terraform_plan = TerraformPlan {
            pending_creation: vec![TerraformResource {
                address: "example.pending_creation".to_string(),
                r#type: "example".to_string(),
                preview: json!({ "example": "pending_creation" }),
            }],
            pending_deletion: vec![TerraformResource {
                address: "example.pending_deletion".to_string(),
                r#type: "example".to_string(),
                preview: json!({ "example": "pending_deletion" }),
            }],
        };

        assert_eq!(expected_terraform_plan, tfplan.into());
    }

    fn simple_tfplan(number_of_types: u32) -> TfPlan {
        let mut changing_resources = vec![];
        for i in 0..number_of_types {
            let create = ChangingResource {
                address: format!("{}.{}", i.to_string(), i.to_string()),
                r#type: i.to_string(),
                change: Change {
                    actions: vec!["create".to_string()],
                    before: None,
                    after: Some(json!({ "foo": i })),
                },
            };
            let delete = ChangingResource {
                address: format!("{}.{}", i.to_string(), i.to_string()),
                r#type: i.to_string(),
                change: Change {
                    actions: vec!["delete".to_string()],
                    before: Some(json!({ "foo": i })),
                    after: None,
                },
            };
            changing_resources.push(create);
            changing_resources.push(delete);
        }
        TfPlan { changing_resources }
    }

    #[test]
    fn q_returns_empty_state() {
        let state = State::ChoosingType(ChoosingType { selected: None });
        let new_state = handle_keypress(&simple_tfplan(1), state, Key::Char('q'));

        assert!(new_state.is_none());
    }

    #[test]
    fn scroll_forwards_through_types() {
        let tfplan = simple_tfplan(3);
        let state = State::ChoosingType(ChoosingType { selected: None });

        let next_0 = handle_keypress(&tfplan, state, Key::Char('j'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(0) })),
            next_0
        );

        let next_1 = handle_keypress(&tfplan, next_0.unwrap(), Key::Char('j'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(1) })),
            next_1
        );

        let next_2 = handle_keypress(&tfplan, next_1.unwrap(), Key::Down);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(2) })),
            next_2
        );

        let next_reset = handle_keypress(&tfplan, next_2.unwrap(), Key::Down);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(0) })),
            next_reset
        );
    }

    #[test]
    fn scroll_backwards_through_types() {
        let tfplan = simple_tfplan(3);
        let state = State::ChoosingType(ChoosingType { selected: None });

        let prev_2 = handle_keypress(&tfplan, state, Key::Char('k'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(2) })),
            prev_2
        );

        let prev_1 = handle_keypress(&tfplan, prev_2.unwrap(), Key::Char('k'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(1) })),
            prev_1
        );

        let prev_0 = handle_keypress(&tfplan, prev_1.unwrap(), Key::Up);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(0) })),
            prev_0
        );

        let prev_reset = handle_keypress(&tfplan, prev_0.unwrap(), Key::Up);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(2) })),
            prev_reset
        );
    }

    #[test]
    fn attempting_to_browse_type_no_type_is_selected_returns_identical_state() {
        let state = State::ChoosingType(ChoosingType { selected: None });

        let new_state = handle_keypress(&simple_tfplan(1), state, Key::Char('\n'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: None })),
            new_state
        );
    }

    #[test]
    fn browsing_type_moves_state_when_type_is_selected() {
        let tfplan = &simple_tfplan(1);
        let state = State::ChoosingType(ChoosingType { selected: Some(0) });

        let new_state = handle_keypress(&tfplan, state, Key::Char('\n'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            r#type: tfplan.unique_types()[0].clone(),
            action_in_scope: TerraformAction::Delete,
            selected_create: None,
            selected_delete: None,
        });

        assert_eq!(Some(expected_state), new_state);
    }

    #[test]
    fn when_browsing_resources_left_puts_delete_action_in_scope() {
        let tfplan = simple_tfplan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let first_press_left = handle_keypress(&tfplan, state, Key::Char('h'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        assert_eq!(Some(expected_state), first_press_left);

        // another left press at this point has no effect
        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });
        assert_eq!(
            Some(expected_state),
            handle_keypress(&tfplan, first_press_left.unwrap(), Key::Left)
        );
    }

    #[test]
    fn when_browsing_resources_right_puts_create_action_in_scope() {
        let tfplan = simple_tfplan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let first_press_right = handle_keypress(&tfplan, state, Key::Char('l'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        assert_eq!(Some(expected_state), first_press_right);

        // another right press at this point has no effect
        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });
        assert_eq!(
            Some(expected_state),
            handle_keypress(&tfplan, first_press_right.unwrap(), Key::Right)
        );
    }
}
