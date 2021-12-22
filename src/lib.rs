use itertools::Itertools;
use serde::Deserialize;
use std::error::Error;
use std::fmt;
use std::io;
use structopt::StructOpt;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::{
    backend::{Backend, TermionBackend},
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

    loop {
        draw(&mut terminal, &model, &plan)?;
        let key = keys
            .next()
            .ok_or_else(|| SanduError::new("Error during keypress event"))??;
        if let Some(new_state) = handle_keypress(&plan, model.state, key) {
            model.state = new_state;
        } else {
            break;
        }
    }
    Ok(())
}

fn draw<B>(
    _terminal: &mut Terminal<B>,
    _model: &Model,
    _plan: &TerraformPlan,
) -> Result<(), Box<dyn Error>>
where
    B: Backend,
{
    Ok(())
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

#[derive(Debug, PartialEq)]
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

fn handle_keypress(plan: &TerraformPlan, state: State, key: Key) -> Option<State> {
    if let Key::Char('q') = key {
        return None;
    }
    match state {
        State::ChoosingType(current) => handle_keypress_while_choosing_type(plan, current, key),
        State::BrowsingResources(current) => {
            handle_keypress_while_browsing_resources(plan, current, key)
        }
    }
}

fn handle_keypress_while_choosing_type(
    plan: &TerraformPlan,
    state: ChoosingType,
    key: Key,
) -> Option<State> {
    match key {
        Key::Char('j') | Key::Down => {
            let next_index = cycle_next(plan.unique_types().len(), &state.selected);
            Some(State::ChoosingType(ChoosingType {
                selected: Some(next_index),
            }))
        }
        Key::Char('k') | Key::Up => {
            let previous_index = cycle_previous(plan.unique_types().len(), &state.selected);
            Some(State::ChoosingType(ChoosingType {
                selected: Some(previous_index),
            }))
        }
        Key::Char('\n') => {
            if let Some(i) = state.selected {
                Some(State::BrowsingResources(BrowsingResources {
                    action_in_scope: TerraformAction::Delete,
                    r#type: plan.unique_types()[i].clone(),
                    selected_create: None,
                    selected_delete: None,
                }))
            } else {
                Some(State::ChoosingType(state))
            }
        }
        _ => Some(State::ChoosingType(state)),
    }
}

fn handle_keypress_while_browsing_resources(
    plan: &TerraformPlan,
    state: BrowsingResources,
    key: Key,
) -> Option<State> {
    match key {
        Key::Char('h') | Key::Left => Some(State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            ..state
        })),
        Key::Char('l') | Key::Right => Some(State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            ..state
        })),
        Key::Char('j') | Key::Down => {
            let (selected_create, selected_delete) = match state.action_in_scope {
                TerraformAction::Create => {
                    let newly_selected_create = cycle_next(
                        resources_of_type(&state.r#type, &plan.pending_creation).len(),
                        &state.selected_create,
                    );
                    (Some(newly_selected_create), state.selected_delete)
                }
                TerraformAction::Delete => {
                    let newly_selected_delete = cycle_next(
                        resources_of_type(&state.r#type, &plan.pending_deletion).len(),
                        &state.selected_delete,
                    );
                    (state.selected_create, Some(newly_selected_delete))
                }
            };
            Some(State::BrowsingResources(BrowsingResources {
                selected_create,
                selected_delete,
                ..state
            }))
        }
        Key::Char('k') | Key::Up => {
            let (selected_create, selected_delete) = match state.action_in_scope {
                TerraformAction::Create => {
                    let newly_selected_create = cycle_previous(
                        resources_of_type(&state.r#type, &plan.pending_creation).len(),
                        &state.selected_create,
                    );
                    (Some(newly_selected_create), state.selected_delete)
                }
                TerraformAction::Delete => {
                    let newly_selected_delete = cycle_previous(
                        resources_of_type(&state.r#type, &plan.pending_deletion).len(),
                        &state.selected_delete,
                    );
                    (state.selected_create, Some(newly_selected_delete))
                }
            };
            Some(State::BrowsingResources(BrowsingResources {
                selected_create,
                selected_delete,
                ..state
            }))
        }
        _ => Some(State::BrowsingResources(state)),
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

#[derive(Debug, PartialEq)]
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
    fn q_returns_empty_state() {
        let state = State::ChoosingType(ChoosingType { selected: None });
        let new_state = handle_keypress(&simple_plan(1), state, Key::Char('q'));

        assert!(new_state.is_none());
    }

    #[test]
    fn scroll_forwards_through_types() {
        let plan = simple_plan(3);
        let state = State::ChoosingType(ChoosingType { selected: None });

        let next_0 = handle_keypress(&plan, state, Key::Char('j'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(0) })),
            next_0
        );

        let next_1 = handle_keypress(&plan, next_0.unwrap(), Key::Char('j'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(1) })),
            next_1
        );

        let next_2 = handle_keypress(&plan, next_1.unwrap(), Key::Down);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(2) })),
            next_2
        );

        let next_reset = handle_keypress(&plan, next_2.unwrap(), Key::Down);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(0) })),
            next_reset
        );
    }

    #[test]
    fn scroll_backwards_through_types() {
        let plan = simple_plan(3);
        let state = State::ChoosingType(ChoosingType { selected: None });

        let prev_2 = handle_keypress(&plan, state, Key::Char('k'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(2) })),
            prev_2
        );

        let prev_1 = handle_keypress(&plan, prev_2.unwrap(), Key::Char('k'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(1) })),
            prev_1
        );

        let prev_0 = handle_keypress(&plan, prev_1.unwrap(), Key::Up);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(0) })),
            prev_0
        );

        let prev_reset = handle_keypress(&plan, prev_0.unwrap(), Key::Up);

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: Some(2) })),
            prev_reset
        );
    }

    #[test]
    fn attempting_to_browse_type_no_type_is_selected_returns_identical_state() {
        let state = State::ChoosingType(ChoosingType { selected: None });

        let new_state = handle_keypress(&simple_plan(1), state, Key::Char('\n'));

        assert_eq!(
            Some(State::ChoosingType(ChoosingType { selected: None })),
            new_state
        );
    }

    #[test]
    fn browsing_type_moves_state_when_type_is_selected() {
        let plan = &simple_plan(1);
        let state = State::ChoosingType(ChoosingType { selected: Some(0) });

        let new_state = handle_keypress(&plan, state, Key::Char('\n'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            r#type: plan.unique_types()[0].clone(),
            action_in_scope: TerraformAction::Delete,
            selected_create: None,
            selected_delete: None,
        });

        assert_eq!(Some(expected_state), new_state);
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

        let first_press_left = handle_keypress(&plan, state, Key::Char('h'));

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
            handle_keypress(&plan, first_press_left.unwrap(), Key::Left)
        );
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

        let first_press_right = handle_keypress(&plan, state, Key::Char('l'));

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
            handle_keypress(&plan, first_press_right.unwrap(), Key::Right)
        );
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

        let first_press_down = handle_keypress(&plan, state, Key::Char('j'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(0),
            selected_delete: None,
        });

        assert_eq!(Some(expected_state), first_press_down);

        let second_press_down = handle_keypress(&plan, first_press_down.unwrap(), Key::Down);

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Create,
            r#type: "0".to_string(),
            selected_create: Some(1),
            selected_delete: None,
        });

        assert_eq!(Some(expected_state), second_press_down);
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

        let first_press_up = handle_keypress(&plan, state, Key::Char('k'));

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: Some(100),
            selected_delete: Some(1),
        });

        assert_eq!(Some(expected_state), first_press_up);

        let second_press_up = handle_keypress(&plan, first_press_up.unwrap(), Key::Up);

        let expected_state = State::BrowsingResources(BrowsingResources {
            action_in_scope: TerraformAction::Delete,
            r#type: "0".to_string(),
            selected_create: Some(100),
            selected_delete: Some(0),
        });

        assert_eq!(Some(expected_state), second_press_up);
    }

    // TODO: if list is empty, do not advance into it
}
