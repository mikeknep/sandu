use itertools::Itertools;
use serde::Deserialize;
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
    widgets::{Block, Borders, List, ListItem, ListState},
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

    let mut model = Model::init(tfplan);

    let stdout = io::stdout().into_raw_mode()?;
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let mut keys = io::stdin().keys();
    terminal.clear()?;

    loop {
        draw(&mut terminal, &mut model)?;
        match get_msg(&keys.next()) {
            Msg::PreviousPane => {
                model.active_pane = model.active_pane.prev();
            }
            Msg::NextPane => {
                model.active_pane = model.active_pane.next();
            }
            Msg::NextListItem => match model.active_pane {
                Pane::TypesList => {
                    cycle_next(model.types(), &mut model.types_list_state);
                }
                _ => {}
            },
            Msg::PreviousListItem => match model.active_pane {
                Pane::TypesList => {
                    cycle_previous(model.types(), &mut model.types_list_state);
                }
                _ => {}
            },
            Msg::Quit => {
                terminal.clear()?;
                break;
            }
            Msg::DoNothing => {}
        }
    }
    return Ok(());
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

        f.render_widget(
            Pane::DestroyingPreview.draw(&model.active_pane),
            resource_preview[0],
        );
        f.render_widget(
            Pane::CreatingPreview.draw(&model.active_pane),
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

fn get_msg(key: &Option<Result<Key, io::Error>>) -> Msg {
    match key {
        Some(Ok(Key::Char('h'))) => Msg::PreviousPane,
        Some(Ok(Key::Char('l'))) => Msg::NextPane,
        Some(Ok(Key::Char('j'))) => Msg::NextListItem,
        Some(Ok(Key::Char('k'))) => Msg::PreviousListItem,
        Some(Ok(Key::Char('q'))) => Msg::Quit,
        _ => Msg::DoNothing,
    }
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

fn cycle_next<T>(items: Vec<T>, state: &mut ListState) {
    let i = match state.selected() {
        Some(i) => {
            if i >= items.len() - 1 {
                0
            } else {
                i + 1
            }
        }
        None => 0,
    };
    state.select(Some(i));
}

fn cycle_previous<T>(items: Vec<T>, state: &mut ListState) {
    let i = match state.selected() {
        Some(i) => {
            if i == 0 {
                items.len() - 1
            } else {
                i - 1
            }
        }
        None => 0,
    };
    state.select(Some(i));
}

#[derive(Debug, PartialEq)]
struct Resource {
    address: String,
    planned_action: PlannedAction,
    preview: serde_json::Value,
    status: Status,
    r#type: String,
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
            planned_action: planned_action,
            preview: preview.clone(),
            r#type: cr.r#type.clone(),
            status: Status::Unstaged,
        })
    }
}

#[derive(Debug, PartialEq)]
enum PlannedAction {
    Create,
    Delete,
}

#[derive(Debug, PartialEq)]
enum Status {
    Staged,
    Unstaged,
}

enum Operation {
    Move { from: String, to: String },
    Import { address: String, identifier: String },
    Remove(String),
}

impl Model {
    fn init(tfplan: TfPlan) -> Model {
        let creates_and_deletes =
            tfplan
                .changing_resources
                .iter()
                .fold((vec![], vec![]), |mut acc, cr| {
                    let conversion: Result<Resource, _> = cr.try_into();
                    match conversion {
                        Ok(resource) => match resource.planned_action {
                            PlannedAction::Create => acc.0.push(resource),
                            PlannedAction::Delete => acc.1.push(resource),
                        },
                        _ => {}
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

    fn selected_type(&self) -> Option<String> {
        match self.types_list_state.selected() {
            Some(i) => Some(self.types()[i].clone()),
            None => None,
        }
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

    fn next(&self) -> Pane {
        match *self {
            Pane::TypesList => Pane::DestroyingList,
            Pane::DestroyingList => Pane::CreatingList,
            Pane::CreatingList => Pane::StagedOperations,
            Pane::StagedOperations => Pane::TypesList,
            _ => panic!("No other panes can ever be active!"),
        }
    }

    fn prev(&self) -> Pane {
        match *self {
            Pane::TypesList => Pane::StagedOperations,
            Pane::DestroyingList => Pane::TypesList,
            Pane::CreatingList => Pane::DestroyingList,
            Pane::StagedOperations => Pane::CreatingList,
            _ => panic!("No other panes can ever be active!"),
        }
    }
}

enum Msg {
    PreviousPane,
    NextPane,
    NextListItem,
    PreviousListItem,
    DoNothing,
    Quit,
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
        let model = Model::init(tfplan);

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
}
