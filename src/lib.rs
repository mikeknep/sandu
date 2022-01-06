use itertools::Itertools;
use serde::Deserialize;
use std::error::Error;
use std::fmt;
use std::io;
use structopt::StructOpt;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::style::Color;
use tui::style::Style;
use tui::text::Span;
use tui::widgets::List;
use tui::widgets::ListItem;
use tui::widgets::ListState;
use tui::widgets::Paragraph;
use tui::{
    backend::{Backend, TermionBackend},
    layout::{Constraint, Direction, Layout, Rect},
    widgets::{Block, Borders},
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
    let plan = serde_json::from_slice::<TfPlan>(&json_bytes)?.into();

    let mut model = Model::new();

    let stdout = io::stdout().into_raw_mode()?;
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let mut keys = io::stdin().keys();
    terminal.clear()?;

    while model.editing() {
        draw(&mut terminal, &model, &plan)?;
        let key = keys
            .next()
            .ok_or_else(|| SanduError::new("Error during keypress event"))??;
        let (new_state, effect) = handle_keypress(&plan, &model.state, key);
        model.accept(new_state, effect);
    }
    Ok(())
}

fn draw<B>(
    terminal: &mut Terminal<B>,
    model: &Model,
    plan: &TerraformPlan,
) -> Result<(), Box<dyn Error>>
where
    B: Backend,
{
    match &model.state {
        State::ChoosingType(state) => draw_choosing_types(terminal, plan, state),
        State::BrowsingResources(state) => draw_browsing_resources(terminal, plan, state),
        State::ConfirmMove(state) => draw_confirm_move(terminal, plan, state),
        _ => Ok(()),
    }
}

fn draw_choosing_types<B>(
    terminal: &mut Terminal<B>,
    plan: &TerraformPlan,
    state: &ChoosingType,
) -> Result<(), Box<dyn Error>>
where
    B: Backend,
{
    terminal.draw(|f| {
        let area = centered_rect(60, 20, f.size());
        let unique_types_list_items: Vec<ListItem> = plan
            .unique_types()
            .iter()
            .map(|t| ListItem::new(Span::raw(t.clone())))
            .collect();
        let (unique_types_list, mut unique_types_list_state) =
            tui_list_for(unique_types_list_items, "Types".to_string(), state.selected);
        f.render_stateful_widget(unique_types_list, area, &mut unique_types_list_state);
    })?;
    Ok(())
}

fn draw_browsing_resources<B>(
    terminal: &mut Terminal<B>,
    plan: &TerraformPlan,
    state: &BrowsingResources,
) -> Result<(), Box<dyn Error>>
where
    B: Backend,
{
    terminal.draw(|f| {
        let panes = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(
                [
                    Constraint::Percentage(25),
                    Constraint::Percentage(25),
                    Constraint::Percentage(25),
                    Constraint::Percentage(25),
                ]
                .as_ref(),
            )
            .split(f.size());
        let deleting_list_pane = panes[0];
        let deleting_preview_pane = panes[1];
        let creating_preview_pane = panes[2];
        let creating_list_pane = panes[3];

        let pending_deletion = resources_of_type(&state.r#type, &plan.pending_deletion);
        let deleting_resources_list_items: Vec<ListItem> = pending_deletion
            .iter()
            .map(|r| ListItem::new(Span::raw(r.address.clone())))
            .collect();
        let (deleting_resources_list, mut deleting_resources_list_state) = tui_list_for(
            deleting_resources_list_items,
            "Deleting".to_string(),
            state.selected_delete,
        );
        f.render_stateful_widget(
            deleting_resources_list,
            deleting_list_pane,
            &mut deleting_resources_list_state,
        );

        if let Some(index) = state.selected_delete {
            let resource = &pending_deletion[index];
            let preview = serde_json::to_string_pretty(&resource.preview).unwrap();
            f.render_widget(
                Paragraph::new(preview).block(Block::default().borders(Borders::ALL)),
                deleting_preview_pane,
            );
        }

        let pending_creation = resources_of_type(&state.r#type, &plan.pending_creation);
        let creating_resources_list_items: Vec<ListItem> = pending_creation
            .iter()
            .map(|r| ListItem::new(Span::raw(r.address.clone())))
            .collect();
        let (creating_resources_list, mut creating_resources_list_state) = tui_list_for(
            creating_resources_list_items,
            "Creating".to_string(),
            state.selected_create,
        );
        f.render_stateful_widget(
            creating_resources_list,
            creating_list_pane,
            &mut creating_resources_list_state,
        );

        if let Some(index) = state.selected_create {
            let resource = &pending_creation[index];
            let preview = serde_json::to_string_pretty(&resource.preview).unwrap();
            f.render_widget(
                Paragraph::new(preview).block(Block::default().borders(Borders::ALL)),
                creating_preview_pane,
            );
        }
    })?;
    Ok(())
}

fn draw_confirm_move<B>(
    terminal: &mut Terminal<B>,
    plan: &TerraformPlan,
    state: &ConfirmMove,
) -> Result<(), Box<dyn Error>>
where
    B: Backend,
{
    terminal.draw(|f| {
        let move_text = format!(
            "
Confirm address change to stage the operation below.


From:  {}

  To:  {}


terraform state mv {} {}
        ",
            state.delete_address, state.create_address, state.delete_address, state.create_address
        );
        let area = centered_rect(60, 20, f.size());
        f.render_widget(
            Paragraph::new(move_text).block(Block::default().borders(Borders::ALL)),
            area,
        );
    })?;
    Ok(())
}

fn tui_list_for(items: Vec<ListItem>, title: String, selected: Option<usize>) -> (List, ListState) {
    let list = List::new(items)
        .block(Block::default().title(title).borders(Borders::ALL))
        .highlight_style(Style::default().bg(Color::Green));
    let mut list_state = ListState::default();
    list_state.select(selected);
    (list, list_state)
}

fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Percentage((100 - percent_y) / 2),
                Constraint::Percentage(percent_y),
                Constraint::Percentage((100 - percent_y) / 2),
            ]
            .as_ref(),
        )
        .split(r);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [
                Constraint::Percentage((100 - percent_x) / 2),
                Constraint::Percentage(percent_x),
                Constraint::Percentage((100 - percent_x) / 2),
            ]
            .as_ref(),
        )
        .split(popup_layout[1])[1]
}

#[derive(Deserialize)]
struct TfPlan {
    #[serde(rename(deserialize = "resource_changes"))]
    changing_resources: Vec<ChangingResource>,
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

#[derive(Clone, Debug, PartialEq)]
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

impl TerraformPlan {
    fn unique_types(&self) -> Vec<String> {
        self.pending_creation
            .iter()
            .chain(self.pending_deletion.iter())
            .map(|resource| resource.r#type.clone())
            .unique()
            .sorted()
            .collect()
    }
}

impl From<TfPlan> for TerraformPlan {
    fn from(tfplan: TfPlan) -> Self {
        let mut pending_creation = vec![];
        let mut pending_deletion = vec![];

        for changing_resource in tfplan.changing_resources {
            match (
                changing_resource.change.actions.len(),
                changing_resource.change.before,
                changing_resource.change.after,
            ) {
                (1, Some(before), None) => {
                    pending_deletion.push(TerraformResource {
                        address: changing_resource.address.clone(),
                        r#type: changing_resource.r#type.clone(),
                        preview: before,
                    });
                }
                (1, None, Some(after)) => {
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

fn resources_of_type(t: &str, resources: &[TerraformResource]) -> Vec<TerraformResource> {
    resources
        .iter()
        .filter(|resource| resource.r#type == t)
        .cloned()
        .collect()
}

pub trait Terraform {
    fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, Box<dyn Error>>;
}

pub trait Filesystem {
    fn file_exists(&self, path: &str) -> bool;
}

#[derive(Clone, Debug, PartialEq)]
struct Model {
    staged_operations: Vec<Operation>,
    state: State,
}

impl Model {
    fn new() -> Self {
        Model {
            staged_operations: vec![],
            state: State::ChoosingType(ChoosingType { selected: None }),
        }
    }

    fn editing(&self) -> bool {
        self.state != State::Finished
    }

    fn accept(&mut self, state: State, effect: Effect) {
        self.state = state;
        match effect {
            Effect::StageOperation(operation) => self.staged_operations.push(operation),
            Effect::NoOp => {}
        }
    }
}

#[derive(Debug, PartialEq)]
enum Effect {
    StageOperation(Operation),
    NoOp,
}

#[derive(Clone, Debug, PartialEq)]
enum TerraformAction {
    Create,
    Delete,
}

#[derive(Clone, Debug, PartialEq)]
struct ChoosingType {
    selected: Option<usize>,
}

#[derive(Clone, Debug, PartialEq)]
struct BrowsingResources {
    action_in_scope: TerraformAction,
    r#type: String,
    selected_create: Option<usize>,
    selected_delete: Option<usize>,
}

#[derive(Clone, Debug, PartialEq)]
struct ConfirmMove {
    delete_address: String,
    create_address: String,
    previous_state: Box<State>,
}

#[derive(Clone, Debug, PartialEq)]
struct ConfirmRemove {
    address: String,
    previous_state: Box<State>,
}

#[derive(Clone, Debug, PartialEq)]
struct ConfirmImport {
    address: String,
    identifier: String,
    previous_state: Box<State>,
}

#[derive(Clone, Debug, PartialEq)]
enum State {
    ChoosingType(ChoosingType),
    BrowsingResources(BrowsingResources),
    ConfirmMove(ConfirmMove),
    ConfirmRemove(ConfirmRemove),
    ConfirmImport(ConfirmImport),
    Finished,
}

fn handle_keypress(plan: &TerraformPlan, state: &State, key: Key) -> (State, Effect) {
    if let Key::Char('q') = key {
        return (State::Finished, Effect::NoOp);
    }
    match state {
        State::ChoosingType(current) => handle_keypress_while_choosing_type(plan, current, key),
        State::BrowsingResources(current) => {
            handle_keypress_while_browsing_resources(plan, current, key)
        }
        State::ConfirmMove(current) => handle_keypress_while_confirming_move(plan, current, key),
        State::ConfirmRemove(current) => {
            handle_keypress_while_confirming_remove(plan, current, key)
        }
        State::ConfirmImport(current) => {
            handle_keypress_while_confirming_import(plan, current, key)
        }
        State::Finished => (State::Finished, Effect::NoOp),
    }
}

fn handle_keypress_while_choosing_type(
    plan: &TerraformPlan,
    state: &ChoosingType,
    key: Key,
) -> (State, Effect) {
    let new_state = match key {
        Key::Char('j') | Key::Down => {
            let next_index = cycle_next(plan.unique_types().len(), &state.selected);
            State::ChoosingType(ChoosingType {
                selected: next_index,
            })
        }
        Key::Char('k') | Key::Up => {
            let previous_index = cycle_previous(plan.unique_types().len(), &state.selected);
            State::ChoosingType(ChoosingType {
                selected: previous_index,
            })
        }
        Key::Char('\n') => {
            if let Some(i) = state.selected {
                State::BrowsingResources(BrowsingResources {
                    action_in_scope: TerraformAction::Delete,
                    r#type: plan.unique_types()[i].clone(),
                    selected_create: None,
                    selected_delete: None,
                })
            } else {
                State::ChoosingType(state.clone())
            }
        }
        _ => State::ChoosingType(state.clone()),
    };
    (new_state, Effect::NoOp)
}

fn handle_keypress_while_browsing_resources(
    plan: &TerraformPlan,
    state: &BrowsingResources,
    key: Key,
) -> (State, Effect) {
    let new_state = match key {
        Key::Char('h') | Key::Left => State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            ..state.clone()
        }),
        Key::Char('l') | Key::Right => State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            ..state.clone()
        }),
        Key::Char('j') | Key::Down => {
            let (selected_create, selected_delete) = match state.action_in_scope {
                TerraformAction::Create => {
                    let newly_selected_create = cycle_next(
                        resources_of_type(&state.r#type, &plan.pending_creation).len(),
                        &state.selected_create,
                    );
                    (newly_selected_create, state.selected_delete)
                }
                TerraformAction::Delete => {
                    let newly_selected_delete = cycle_next(
                        resources_of_type(&state.r#type, &plan.pending_deletion).len(),
                        &state.selected_delete,
                    );
                    (state.selected_create, newly_selected_delete)
                }
            };
            State::BrowsingResources(BrowsingResources {
                selected_create,
                selected_delete,
                ..state.clone()
            })
        }
        Key::Char('k') | Key::Up => {
            let (selected_create, selected_delete) = match state.action_in_scope {
                TerraformAction::Create => {
                    let newly_selected_create = cycle_previous(
                        resources_of_type(&state.r#type, &plan.pending_creation).len(),
                        &state.selected_create,
                    );
                    (newly_selected_create, state.selected_delete)
                }
                TerraformAction::Delete => {
                    let newly_selected_delete = cycle_previous(
                        resources_of_type(&state.r#type, &plan.pending_deletion).len(),
                        &state.selected_delete,
                    );
                    (state.selected_create, newly_selected_delete)
                }
            };
            State::BrowsingResources(BrowsingResources {
                selected_create,
                selected_delete,
                ..state.clone()
            })
        }
        Key::Char('m') => {
            if let (Some(c), Some(d)) = (state.selected_create, state.selected_delete) {
                let create_address = resources_of_type(&state.r#type, &plan.pending_creation)[c]
                    .address
                    .clone();
                let delete_address = resources_of_type(&state.r#type, &plan.pending_deletion)[d]
                    .address
                    .clone();
                State::ConfirmMove(ConfirmMove {
                    previous_state: Box::new(State::BrowsingResources(state.clone())),
                    create_address,
                    delete_address,
                })
            } else {
                State::BrowsingResources(state.clone())
            }
        }
        Key::Char('r') => {
            if let Some(d) = state.selected_delete {
                let address = resources_of_type(&state.r#type, &plan.pending_deletion)[d]
                    .address
                    .clone();
                State::ConfirmRemove(ConfirmRemove {
                    previous_state: Box::new(State::BrowsingResources(state.clone())),
                    address,
                })
            } else {
                State::BrowsingResources(state.clone())
            }
        }
        Key::Char('i') => {
            if let Some(c) = state.selected_create {
                let address = resources_of_type(&state.r#type, &plan.pending_creation)[c]
                    .address
                    .clone();
                State::ConfirmImport(ConfirmImport {
                    previous_state: Box::new(State::BrowsingResources(state.clone())),
                    identifier: "".to_string(),
                    address,
                })
            } else {
                State::BrowsingResources(state.clone())
            }
        }
        Key::Backspace => State::ChoosingType(ChoosingType { selected: None }),
        _ => State::BrowsingResources(state.clone()),
    };
    (new_state, Effect::NoOp)
}

fn handle_keypress_while_confirming_move(
    _plan: &TerraformPlan,
    state: &ConfirmMove,
    key: Key,
) -> (State, Effect) {
    match key {
        Key::Backspace => (*state.previous_state.clone(), Effect::NoOp),
        Key::Char('\n') => {
            let operation = Operation::Move {
                from: state.delete_address.clone(),
                to: state.create_address.clone(),
            };
            (State::Finished, Effect::StageOperation(operation))
        }
        _ => (State::ConfirmMove(state.clone()), Effect::NoOp),
    }
}

fn handle_keypress_while_confirming_remove(
    _plan: &TerraformPlan,
    state: &ConfirmRemove,
    key: Key,
) -> (State, Effect) {
    match key {
        Key::Backspace => (*state.previous_state.clone(), Effect::NoOp),
        Key::Char('\n') => {
            let operation = Operation::Remove(state.address.clone());
            (State::Finished, Effect::StageOperation(operation))
        }
        _ => (State::ConfirmRemove(state.clone()), Effect::NoOp),
    }
}

fn handle_keypress_while_confirming_import(
    _plan: &TerraformPlan,
    state: &ConfirmImport,
    key: Key,
) -> (State, Effect) {
    match key {
        Key::Backspace => match state.identifier.len() {
            0 => (*state.previous_state.clone(), Effect::NoOp),
            _ => {
                let mut identifier = state.identifier.clone();
                identifier.pop();
                let new_state = State::ConfirmImport(ConfirmImport {
                    identifier,
                    ..state.clone()
                });
                (new_state, Effect::NoOp)
            }
        },
        Key::Char('\n') => {
            let operation = Operation::Import {
                address: state.address.clone(),
                identifier: state.identifier.clone(),
            };
            (State::Finished, Effect::StageOperation(operation))
        }
        Key::Char(c) => {
            let mut identifier = state.identifier.clone();
            identifier.push(c);
            let new_state = State::ConfirmImport(ConfirmImport {
                identifier,
                ..state.clone()
            });
            (new_state, Effect::NoOp)
        }
        _ => (State::ConfirmImport(state.clone()), Effect::NoOp),
    }
}

fn cycle_next(items_length: usize, selected: &Option<usize>) -> Option<usize> {
    if items_length == 0 {
        return None;
    }
    match selected {
        Some(i) => {
            if *i >= items_length - 1 {
                Some(0)
            } else {
                Some(i + 1)
            }
        }
        None => Some(0),
    }
}

fn cycle_previous(items_length: usize, selected: &Option<usize>) -> Option<usize> {
    if items_length == 0 {
        return None;
    }
    match selected {
        Some(i) => {
            if *i == 0 {
                Some(items_length - 1)
            } else {
                Some(i - 1)
            }
        }
        None => Some(items_length - 1),
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Operation {
    Move { from: String, to: String },
    Import { address: String, identifier: String },
    Remove(String),
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

    fn simple_plan(number_of_types: u32) -> TerraformPlan {
        let mut pending_creation = vec![];
        let mut pending_deletion = vec![];
        for i in 0..number_of_types {
            pending_creation.push(TerraformResource {
                address: format!("{}.{}", i.to_string(), i.to_string()),
                r#type: i.to_string(),
                preview: json!({}),
            });
            pending_deletion.push(TerraformResource {
                address: format!("{}.{}", i.to_string(), i.to_string()),
                r#type: i.to_string(),
                preview: json!({}),
            });
        }
        TerraformPlan {
            pending_creation,
            pending_deletion,
        }
    }

    #[test]
    fn terraform_plan_can_list_unique_types_in_alphabetical_order() {
        let terraform_plan = TerraformPlan {
            pending_creation: vec![TerraformResource {
                address: "example.create".to_string(),
                r#type: "two".to_string(),
                preview: json!({}),
            }],
            pending_deletion: vec![TerraformResource {
                address: "example.delete".to_string(),
                r#type: "one".to_string(),
                preview: json!({}),
            }],
        };

        assert_eq!(
            vec!["one".to_string(), "two".to_string()],
            terraform_plan.unique_types()
        );
    }

    #[test]
    fn model_accepts_new_states() {
        let mut model = Model::new();
        let new_state = State::ChoosingType(ChoosingType {
            selected: Some(100),
        });
        model.accept(new_state.clone(), Effect::NoOp);

        assert_eq!(new_state, model.state);
    }

    #[test]
    fn no_op_effect_does_not_alter_model() {
        let original_model = Model::new();
        let mut model = original_model.clone();

        model.accept(original_model.state.clone(), Effect::NoOp);

        assert_eq!(original_model, model);
    }

    #[test]
    fn q_returns_finished_state() {
        let state = State::ChoosingType(ChoosingType { selected: None });
        let (new_state, _) = handle_keypress(&simple_plan(1), &state, Key::Char('q'));

        assert_eq!(State::Finished, new_state);
    }

    #[test]
    fn scroll_forwards_through_types() {
        let plan = simple_plan(3);
        let state = State::ChoosingType(ChoosingType { selected: None });

        let (next_0, _) = handle_keypress(&plan, &state, Key::Char('j'));

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(0) }),
            next_0
        );

        let (next_1, _) = handle_keypress(&plan, &next_0, Key::Char('j'));

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(1) }),
            next_1
        );

        let (next_2, _) = handle_keypress(&plan, &next_1, Key::Down);

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(2) }),
            next_2
        );

        let (next_reset, _) = handle_keypress(&plan, &next_2, Key::Down);

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(0) }),
            next_reset
        );
    }

    #[test]
    fn scroll_backwards_through_types() {
        let plan = simple_plan(3);
        let state = State::ChoosingType(ChoosingType { selected: None });

        let (prev_2, _) = handle_keypress(&plan, &state, Key::Char('k'));

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(2) }),
            prev_2
        );

        let (prev_1, _) = handle_keypress(&plan, &prev_2, Key::Char('k'));

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(1) }),
            prev_1
        );

        let (prev_0, _) = handle_keypress(&plan, &prev_1, Key::Up);

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(0) }),
            prev_0
        );

        let (prev_reset, _) = handle_keypress(&plan, &prev_0, Key::Up);

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: Some(2) }),
            prev_reset
        );
    }

    #[test]
    fn attempting_to_browse_type_no_type_is_selected_returns_identical_state() {
        let state = State::ChoosingType(ChoosingType { selected: None });

        let (new_state, _) = handle_keypress(&simple_plan(1), &state, Key::Char('\n'));

        assert_eq!(
            State::ChoosingType(ChoosingType { selected: None }),
            new_state
        );
    }

    #[test]
    fn browsing_type_moves_state_when_type_is_selected() {
        let plan = &simple_plan(1);
        let state = State::ChoosingType(ChoosingType { selected: Some(0) });

        let (new_state, _) = handle_keypress(&plan, &state, Key::Char('\n'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            r#type: plan.unique_types()[0].clone(),
            action_in_scope: TerraformAction::Delete,
            selected_create: None,
            selected_delete: None,
        });

        assert_eq!(expected_state, new_state);
    }

    #[test]
    fn when_browsing_resources_backspace_returns_to_choosing_type() {
        let plan = simple_plan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let (new_state, _) = handle_keypress(&plan, &state, Key::Backspace);

        let expected_state = State::ChoosingType(ChoosingType { selected: None });
        assert_eq!(expected_state, new_state);
    }

    #[test]
    fn when_browsing_resources_left_puts_delete_action_in_scope() {
        let plan = simple_plan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let (first_press_left, _) = handle_keypress(&plan, &state, Key::Char('h'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        assert_eq!(expected_state, first_press_left);

        // another left press at this point has no effect
        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let (next_press_left, _) = handle_keypress(&plan, &first_press_left, Key::Left);

        assert_eq!(expected_state, next_press_left);
    }

    #[test]
    fn when_browsing_resources_right_puts_create_action_in_scope() {
        let plan = simple_plan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let (first_press_right, _) = handle_keypress(&plan, &state, Key::Char('l'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        assert_eq!(expected_state, first_press_right);

        // another right press at this point has no effect
        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let (next_press_right, _) = handle_keypress(&plan, &first_press_right, Key::Right);

        assert_eq!(expected_state, next_press_right);
    }

    #[test]
    fn when_browsing_resources_down_scrolls_foward_jn_list_of_resources_for_action_in_scope() {
        let mut plan = simple_plan(1);
        plan.pending_creation.push(TerraformResource {
            address: "additional.resource".to_string(),
            preview: json!({}),
            r#type: "0".to_string(),
        });

        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        });

        let (first_press_down, _) = handle_keypress(&plan, &state, Key::Char('j'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(0),
            selected_delete: None,
        });

        assert_eq!(expected_state, first_press_down);

        let (second_press_down, _) = handle_keypress(&plan, &first_press_down, Key::Down);

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(1),
            selected_delete: None,
        });

        assert_eq!(expected_state, second_press_down);
    }

    #[test]
    fn when_browsing_resources_up_scrolls_backward_jn_list_of_resources_for_action_in_scope() {
        let mut plan = simple_plan(1);
        plan.pending_deletion.push(TerraformResource {
            address: "additional.resource".to_string(),
            preview: json!({}),
            r#type: "0".to_string(),
        });

        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: Some(100),
            selected_delete: None,
        });

        let (first_press_up, _) = handle_keypress(&plan, &state, Key::Char('k'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: Some(100),
            selected_delete: Some(1),
        });

        assert_eq!(expected_state, first_press_up);

        let (second_press_up, _) = handle_keypress(&plan, &first_press_up, Key::Up);

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: Some(100),
            selected_delete: Some(0),
        });

        assert_eq!(expected_state, second_press_up);
    }

    #[test]
    fn cannot_cycle_into_an_empty_list() {
        assert!(cycle_next(0, &None).is_none());
        assert!(cycle_previous(0, &None).is_none());
    }

    #[test]
    fn proposing_a_state_move_operation_requires_selected_create_and_delete() {
        let plan = simple_plan(1);
        let blank_browsing_resources_state = BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        };
        let nothing_selected = State::BrowsingResources(blank_browsing_resources_state.clone());
        let only_delete_selected = State::BrowsingResources(BrowsingResources {
            selected_delete: Some(0),
            ..blank_browsing_resources_state.clone()
        });
        let only_create_selected = State::BrowsingResources(BrowsingResources {
            selected_create: Some(0),
            ..blank_browsing_resources_state.clone()
        });

        let (nothing_selected_press, _) = handle_keypress(&plan, &nothing_selected, Key::Char('m'));
        let (only_delete_selected_press, _) =
            handle_keypress(&plan, &only_delete_selected, Key::Char('m'));
        let (only_create_selected_press, _) =
            handle_keypress(&plan, &only_create_selected, Key::Char('m'));

        assert_eq!(nothing_selected, nothing_selected_press);

        assert_eq!(only_delete_selected, only_delete_selected_press);

        assert_eq!(only_create_selected, only_create_selected_press);
    }

    #[test]
    fn proposing_a_remove_operation_requires_selected_delete() {
        let plan = simple_plan(1);
        let blank_browsing_resources_state = BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        };
        let nothing_selected = State::BrowsingResources(blank_browsing_resources_state.clone());
        let only_create_selected = State::BrowsingResources(BrowsingResources {
            selected_create: Some(0),
            ..blank_browsing_resources_state.clone()
        });

        let (nothing_selected_press, _) = handle_keypress(&plan, &nothing_selected, Key::Char('m'));
        let (only_create_selected_press, _) =
            handle_keypress(&plan, &only_create_selected, Key::Char('r'));

        assert_eq!(nothing_selected, nothing_selected_press);

        assert_eq!(only_create_selected, only_create_selected_press);
    }

    #[test]
    fn proposing_an_import_operation_requires_selected_crete() {
        let plan = simple_plan(1);
        let blank_browsing_resources_state = BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: None,
        };
        let nothing_selected = State::BrowsingResources(blank_browsing_resources_state.clone());
        let only_delete_selected = State::BrowsingResources(BrowsingResources {
            selected_delete: Some(0),
            ..blank_browsing_resources_state.clone()
        });

        let (nothing_selected_press, _) = handle_keypress(&plan, &nothing_selected, Key::Char('m'));
        let (only_delete_selected_press, _) =
            handle_keypress(&plan, &only_delete_selected, Key::Char('i'));

        assert_eq!(nothing_selected, nothing_selected_press);

        assert_eq!(only_delete_selected, only_delete_selected_press);
    }

    #[test]
    fn propose_an_import_operation() {
        let plan = simple_plan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(0),
            selected_delete: None,
        });

        let expected_state = State::ConfirmImport(ConfirmImport {
            identifier: "".to_string(),
            address: "0.0".to_string(),
            previous_state: Box::new(state.clone()),
        });

        let (confirm_import_state, _) = handle_keypress(&plan, &state, Key::Char('i'));

        assert_eq!(expected_state, confirm_import_state);
    }

    #[test]
    fn propose_a_remove_operation() {
        let plan = simple_plan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: Some(0),
        });

        let expected_state = State::ConfirmRemove(ConfirmRemove {
            address: "0.0".to_string(),
            previous_state: Box::new(state.clone()),
        });

        let (confirm_remove_state, _) = handle_keypress(&plan, &state, Key::Char('r'));

        assert_eq!(expected_state, confirm_remove_state);
    }

    #[test]
    fn propose_a_state_move_operation() {
        let plan = simple_plan(1);
        let state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(0),
            selected_delete: Some(0),
        });

        let expected_state = State::ConfirmMove(ConfirmMove {
            delete_address: "0.0".to_string(),
            create_address: "0.0".to_string(),
            previous_state: Box::new(state.clone()),
        });

        let (confirm_move_state, _) = handle_keypress(&plan, &state, Key::Char('m'));

        assert_eq!(expected_state, confirm_move_state);
    }

    #[test]
    fn aborting_a_state_move_operation_returns_to_previous_state() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(0),
            selected_delete: Some(0),
        });

        let state = State::ConfirmMove(ConfirmMove {
            delete_address: "0.0".to_string(),
            create_address: "0.0".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        });

        let (abort_state, _) = handle_keypress(&plan, &state, Key::Backspace);

        assert_eq!(browsing_state, abort_state);
    }

    #[test]
    fn confirming_a_state_move_sends_a_stage_operation_effect_with_a_move() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(0),
            selected_delete: Some(0),
        });
        let state = State::ConfirmMove(ConfirmMove {
            delete_address: "from".to_string(),
            create_address: "to".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        });

        let (_, effect) = handle_keypress(&plan, &state, Key::Char('\n'));

        assert_eq!(
            Effect::StageOperation(Operation::Move {
                from: "from".to_string(),
                to: "to".to_string()
            }),
            effect
        );
    }

    #[test]
    fn aborting_a_remove_operation_returns_to_previous_state() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: Some(0),
        });

        let state = State::ConfirmRemove(ConfirmRemove {
            address: "0.0".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        });

        let (abort_state, _) = handle_keypress(&plan, &state, Key::Backspace);

        assert_eq!(browsing_state, abort_state);
    }

    #[test]
    fn confirming_a_remove_sends_a_stage_operation_effect_with_a_remove() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: Some(0),
        });
        let state = State::ConfirmRemove(ConfirmRemove {
            address: "remove".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        });

        let (_, effect) = handle_keypress(&plan, &state, Key::Char('\n'));

        assert_eq!(
            Effect::StageOperation(Operation::Remove("remove".to_string())),
            effect
        );
    }

    #[test]
    fn aborting_an_import_operation_returns_to_previous_state_if_identifier_is_empty() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(0),
            selected_delete: None,
        });

        let state = State::ConfirmImport(ConfirmImport {
            address: "0.0".to_string(),
            identifier: "".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        });

        let (abort_state, _) = handle_keypress(&plan, &state, Key::Backspace);

        assert_eq!(browsing_state, abort_state);
    }

    #[test]
    fn backspacing_while_identifer_is_not_empty_removes_from_identifier() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: Some(0),
        });
        let import = ConfirmImport {
            address: "import".to_string(),
            identifier: "a".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        };
        let initial_state = State::ConfirmImport(import.clone());

        let (identifier_empty, _) = handle_keypress(&plan, &initial_state, Key::Backspace);

        assert_eq!(
            State::ConfirmImport(ConfirmImport {
                identifier: "".to_string(),
                ..import.clone()
            }),
            identifier_empty.clone()
        );
    }

    #[test]
    fn typing_during_confirm_import_appends_to_identifier() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: Some(0),
        });
        let import = ConfirmImport {
            address: "import".to_string(),
            identifier: "".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        };
        let initial_state = State::ConfirmImport(import.clone());

        let (identifier_a, _) = handle_keypress(&plan, &initial_state, Key::Char('a'));

        assert_eq!(
            State::ConfirmImport(ConfirmImport {
                identifier: "a".to_string(),
                ..import.clone()
            }),
            identifier_a.clone()
        );
    }

    #[test]
    fn confirming_an_import_sends_a_stage_operation_effect_with_an_import() {
        let plan = simple_plan(1);
        let browsing_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: None,
            selected_delete: Some(0),
        });
        let state = State::ConfirmImport(ConfirmImport {
            address: "import".to_string(),
            identifier: "identifier".to_string(),
            previous_state: Box::new(browsing_state.clone()),
        });

        let (_, effect) = handle_keypress(&plan, &state, Key::Char('\n'));

        assert_eq!(
            Effect::StageOperation(Operation::Import {
                address: "import".to_string(),
                identifier: "identifier".to_string()
            }),
            effect
        );
    }

    #[test]
    #[ignore]
    fn confirming_a_state_move_progresses_to_the_next_state() {
        todo!("Not sure where we'll go from here just yet!");
    }

    #[test]
    fn accepting_a_stage_operation_effect_adds_the_operation_to_the_model() {
        let mut model = Model::new();
        let new_state = State::ChoosingType(ChoosingType { selected: None });
        let operation = Operation::Move {
            from: "from".to_string(),
            to: "to".to_string(),
        };
        let stage_operation_effect = Effect::StageOperation(operation.clone());

        model.accept(new_state, stage_operation_effect);

        assert_eq!(1, model.staged_operations.len());
        assert_eq!(operation, model.staged_operations[0]);
    }
}
