use crate::terraform::{Terraform, TerraformAction, TerraformPlan, TerraformResource};
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
use tui::widgets::{Clear, List, ListItem, ListState, Paragraph};
use tui::{
    backend::{Backend, TermionBackend},
    layout::{Constraint, Direction, Layout, Rect},
    widgets::{Block, Borders},
    Terminal,
};

pub mod terraform;

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
    let plan = clients.terraform.show_plan(&sandu.planfile)?;

    let mut model = Model::new();

    let stdout = io::stdout().into_raw_mode()?;
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let mut keys = io::stdin().keys();
    terminal.clear()?;

    while model.in_progress() {
        draw(&mut terminal, &model, &plan)?;
        let key = keys
            .next()
            .ok_or_else(|| SanduError::new("Error during keypress event"))??;
        let (nav, effect) = keypress(&plan, &model, key);
        model.navigation = nav;
        model.accept(effect);
    }

    terminal.clear()?;
    for operation in model.staged_operations {
        println!("{}\r", operation);
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
    terminal.draw(|f| {
        // Establish the basic visual layout of panes
        let columns = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(
                [
                    Constraint::Percentage(40),
                    Constraint::Percentage(30),
                    Constraint::Percentage(30),
                ]
                .as_ref(),
            )
            .split(f.size());
        let left_column = columns[0];
        let deleting_column = columns[1];
        let creating_column = columns[2];

        let left_panes = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Percentage(40),
                    Constraint::Percentage(20),
                    Constraint::Percentage(40),
                ]
                .as_ref(),
            )
            .split(left_column);
        let types_list_pane = left_panes[0];
        let confirmation_pane = left_panes[1];
        let staged_operations_list_pane = left_panes[2];

        let deleting_panes = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(35), Constraint::Percentage(65)].as_ref())
            .split(deleting_column);
        let deleting_list_pane = deleting_panes[0];
        let deleting_preview_pane = deleting_panes[1];

        let creating_panes = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(35), Constraint::Percentage(65)].as_ref())
            .split(creating_column);
        let creating_list_pane = creating_panes[0];
        let creating_preview_pane = creating_panes[1];

        let modal_pane = centered_rect(60, 40, f.size());

        // Populate panes with content

        let types_list_items: Vec<ListItem> = plan
            .unique_types()
            .iter()
            .map(|t| ListItem::new(Span::raw(t.clone())))
            .collect();
        let (types_list, mut types_list_state) = tui_list_for(
            types_list_items,
            "Types".to_string(),
            model.navigation.selected_type,
            model.navigation.active_list.is_types_list(),
        );
        f.render_stateful_widget(types_list, types_list_pane, &mut types_list_state);

        let staged_operations_list_items: Vec<ListItem> = model
            .staged_operations
            .iter()
            .map(|o| ListItem::new(Span::raw(o.to_string())))
            .collect();
        let (staged_operations_list, mut staged_operations_list_state) = tui_list_for(
            staged_operations_list_items,
            "Staged operations".to_string(),
            model.navigation.selected_operation,
            model.navigation.active_list.is_staged_operations_list(),
        );
        f.render_stateful_widget(
            staged_operations_list,
            staged_operations_list_pane,
            &mut staged_operations_list_state,
        );

        if let Some(selected_type_index) = model.navigation.selected_type {
            let active_type = plan.unique_types()[selected_type_index].clone();
            let pending_deletion = unstaged_resources_of_type(
                &active_type,
                &plan.pending_deletion,
                &model.staged_operations,
            );
            let pending_creation = unstaged_resources_of_type(
                &active_type,
                &plan.pending_creation,
                &model.staged_operations,
            );

            let deleting_list_items: Vec<ListItem> = pending_deletion
                .iter()
                .map(|r| ListItem::new(Span::raw(r.address.clone())))
                .collect();
            let creating_list_items: Vec<ListItem> = pending_creation
                .iter()
                .map(|r| ListItem::new(Span::raw(r.address.clone())))
                .collect();

            let (deleting_list, mut deleting_list_state) = tui_list_for(
                deleting_list_items,
                "Deleting".to_string(),
                model.navigation.selected_delete,
                model.navigation.active_list.is_delete_list(),
            );
            let (creating_list, mut creating_list_state) = tui_list_for(
                creating_list_items,
                "Creating".to_string(),
                model.navigation.selected_create,
                model.navigation.active_list.is_create_list(),
            );

            f.render_stateful_widget(deleting_list, deleting_list_pane, &mut deleting_list_state);
            f.render_stateful_widget(creating_list, creating_list_pane, &mut creating_list_state);

            // Resource previews
            if let Some(selected_delete_index) = model.navigation.selected_delete {
                let delete_resource = &pending_deletion[selected_delete_index];
                let delete_preview =
                    serde_json::to_string_pretty(&delete_resource.preview).unwrap();
                f.render_widget(
                    Paragraph::new(delete_preview).block(
                        Block::default()
                            .borders(Borders::ALL)
                            .border_style(Style::default().fg(Color::Gray)),
                    ),
                    deleting_preview_pane,
                );
            }
            if let Some(selected_create_index) = model.navigation.selected_create {
                let create_resource = &pending_creation[selected_create_index];
                let create_preview =
                    serde_json::to_string_pretty(&create_resource.preview).unwrap();
                f.render_widget(
                    Paragraph::new(create_preview).block(
                        Block::default()
                            .borders(Borders::ALL)
                            .border_style(Style::default().fg(Color::Gray)),
                    ),
                    creating_preview_pane,
                );
            }
        }

        // Populate confirmation pane while confirming an operation
        if let ActionState::Confirming(operation) = &model.action_state {
            let confirmation_text = get_confirmation_text(operation);
            let confirmation = Paragraph::new(confirmation_text)
                .block(Block::default().title("Confirm").borders(Borders::ALL));
            f.render_widget(confirmation, confirmation_pane);
        }

        // Populate help modal when seeking help
        if let ActionState::SeekingHelp(previous_action_state) = &model.action_state {
            let help_text = contextual_help_text(*(previous_action_state.clone()));
            f.render_widget(Clear, modal_pane);
            f.render_widget(
                Paragraph::new(help_text).block(
                    Block::default()
                        .title("Help".to_string())
                        .borders(Borders::ALL),
                ),
                modal_pane,
            );
        }
    })?;
    Ok(())
}

fn get_confirmation_text(operation: &Operation) -> String {
    match operation {
        Operation::Remove(address) => format!(
            "
Confirm REMOVE operation.
(Causes Terraform to \"forget\" the resource without destroying it.)


Address: {}


terraform state rm {}
        ",
            address, address
        ),
        Operation::Import {
            address,
            identifier,
        } => format!(
            "
Confirm IMPORT operation.
(Pulls existing resource into Terraform management.)


Address:    {}

Identifier: {}


terraform import {} {}
        ",
            address, identifier, address, identifier
        ),
        Operation::Move { from, to } => format!(
            "
Confirm MOVE operation.
(Changes the Terraform address without altering the resource itself.)


From: {}

To:   {}


terraform state mv {} {}
        ",
            from, to, from, to
        ),
    }
}

const NAVIGATION_HELP: &str = "
Move to:                                Stage an operation:
(t) Types list                          (i) Import
(s) Staged operations list              (m) Move
(d) Deleting resources list             (r) Remove
(c) Creating resources list

Scroll with up/down arrows, j/k (vim-style), or ctrl-n/p

Exit program with 'q' or Esc

Close this help menu via 'h' or '?'
";

const CONFIRMING_REMOVE_HELP: &str = "
Stage the operation: Enter/Return
Abort and go back:   Delete

Close this help menu via 'h' or '?'
";
const CONFIRMING_MOVE_HELP: &str = "
Stage the operation: Enter/Return
Abort and go back:   Delete

Close this help menu via 'h' or '?'
";
const CONFIRMING_IMPORT_HELP: &str = "
Type in the resource identifier.

Stage the operation: Enter/Return
Abort and go back:   Delete

Close this help menu via 'h' or '?'
";

fn contextual_help_text(action_state: ActionState) -> String {
    match action_state {
        ActionState::Navigating => NAVIGATION_HELP.to_string(),
        ActionState::Confirming(operation) => help_text_for_operation(operation),
        _ => unreachable!(),
    }
}

fn help_text_for_operation(operation: Operation) -> String {
    match operation {
        Operation::Remove(_) => CONFIRMING_REMOVE_HELP.to_string(),
        Operation::Move { .. } => CONFIRMING_MOVE_HELP.to_string(),
        Operation::Import { .. } => CONFIRMING_IMPORT_HELP.to_string(),
    }
}

fn tui_list_for(
    items: Vec<ListItem>,
    title: String,
    selected: Option<usize>,
    is_active: bool,
) -> (List, ListState) {
    let border_color = if is_active {
        Color::Yellow
    } else {
        Color::Gray
    };
    let list = List::new(items)
        .block(
            Block::default()
                .title(title)
                .borders(Borders::ALL)
                .border_style(Style::default().fg(border_color)),
        )
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

fn unstaged_resources_of_type(
    t: &str,
    resources: &[TerraformResource],
    staged_operations: &[Operation],
) -> Vec<TerraformResource> {
    let staged_resource_addresses: Vec<String> = staged_operations
        .iter()
        .flat_map(|operation| operation.addresses())
        .collect();

    resources
        .iter()
        .filter(|resource| {
            resource.r#type == t && !staged_resource_addresses.contains(&resource.address)
        })
        .cloned()
        .collect()
}

fn selected_resource(
    plan: &TerraformPlan,
    action: TerraformAction,
    selected_type_index: Option<usize>,
    selected_resource_index: Option<usize>,
    operations: &[Operation],
) -> Option<TerraformResource> {
    if let Some(i) = selected_type_index {
        let resource_type = &plan.unique_types()[i];
        let resources = match action {
            TerraformAction::Create => &plan.pending_creation,
            TerraformAction::Delete => &plan.pending_deletion,
        };
        selected_resource_index
            .map(|i| unstaged_resources_of_type(resource_type, resources, operations)[i].clone())
    } else {
        None
    }
}

pub trait Filesystem {
    fn file_exists(&self, path: &str) -> bool;
}

#[derive(Clone, Debug, PartialEq)]
enum ActionState {
    Navigating,
    Confirming(Operation),
    SeekingHelp(Box<ActionState>),
    Exiting,
}

#[derive(Clone, Debug, PartialEq)]
enum NavigationList {
    Types,
    Create(String),
    Delete(String),
    StagedOperations,
}

impl NavigationList {
    fn is_types_list(&self) -> bool {
        matches!(self, NavigationList::Types)
    }

    fn is_create_list(&self) -> bool {
        matches!(self, NavigationList::Create(_))
    }

    fn is_delete_list(&self) -> bool {
        matches!(self, NavigationList::Delete(_))
    }

    fn is_staged_operations_list(&self) -> bool {
        matches!(self, NavigationList::StagedOperations)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Navigation {
    selected_type: Option<usize>,
    selected_create: Option<usize>,
    selected_delete: Option<usize>,
    selected_operation: Option<usize>,
    active_list: NavigationList,
}

impl Navigation {
    fn default() -> Self {
        Navigation {
            selected_type: None,
            selected_create: None,
            selected_delete: None,
            selected_operation: None,
            active_list: NavigationList::Types,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Model {
    action_state: ActionState,
    navigation: Navigation,
    staged_operations: Vec<Operation>,
}

impl Model {
    fn new() -> Self {
        Model {
            action_state: ActionState::Navigating,
            navigation: Navigation::default(),
            staged_operations: vec![],
        }
    }

    fn in_progress(&self) -> bool {
        self.action_state != ActionState::Exiting
    }

    fn accept(&mut self, effect: Effect) {
        match effect {
            Effect::SeekConfirmation(operation) => {
                self.action_state = ActionState::Confirming(operation)
            }
            Effect::CloseConfirmationModal => self.action_state = ActionState::Navigating,
            Effect::StageOperation(operation) => {
                self.staged_operations.push(operation);
                self.action_state = ActionState::Navigating;
            }
            Effect::UnstageOperation(index) => {
                self.staged_operations.remove(index);
            }
            Effect::OpenHelpModal => {
                let previous_action_state = self.action_state.clone();
                self.action_state = ActionState::SeekingHelp(Box::new(previous_action_state));
            }
            Effect::CloseHelpModal(action_state) => self.action_state = action_state,
            Effect::Exit => self.action_state = ActionState::Exiting,
            Effect::NoOp => {}
        }
    }
}

#[derive(Debug, PartialEq)]
enum Effect {
    SeekConfirmation(Operation),
    CloseConfirmationModal,
    StageOperation(Operation),
    UnstageOperation(usize),
    OpenHelpModal,
    CloseHelpModal(ActionState),
    Exit,
    NoOp,
}

fn keypress(plan: &TerraformPlan, model: &Model, key: Key) -> (Navigation, Effect) {
    if let Key::Esc | Key::Char('q') = key {
        return (model.navigation.clone(), Effect::Exit);
    }
    if let Key::Char('h') | Key::Char('?') = key {
        return toggle_help(model);
    }
    match &model.action_state {
        ActionState::Navigating => {
            keypress_while_navigating(plan, &model.navigation, &model.staged_operations, key)
        }
        ActionState::Confirming(operation) => {
            keypress_while_confirming(&model.navigation, operation, key)
        }
        ActionState::SeekingHelp(_) => (model.navigation.clone(), Effect::NoOp),
        ActionState::Exiting => unreachable!(),
    }
}

fn toggle_help(model: &Model) -> (Navigation, Effect) {
    match &model.action_state {
        ActionState::SeekingHelp(prev) => (
            model.navigation.clone(),
            Effect::CloseHelpModal(*(prev.clone())),
        ),
        _ => (model.navigation.clone(), Effect::OpenHelpModal),
    }
}

fn keypress_while_navigating(
    plan: &TerraformPlan,
    navigation: &Navigation,
    operations: &[Operation],
    key: Key,
) -> (Navigation, Effect) {
    match key {
        // scroll through active list
        Key::Char('j') | Key::Down | Key::Ctrl('n') => scroll_next(plan, navigation, operations),
        Key::Char('k') | Key::Up | Key::Ctrl('p') => scroll_previous(plan, navigation, operations),

        // deselect from list
        Key::Backspace => deselect_list_item(navigation),

        // change the active list
        Key::Char('t') => move_to(navigation, NavigationList::Types),
        Key::Char('s') => move_to(navigation, NavigationList::StagedOperations),
        Key::Char('c') => {
            if let Some(i) = navigation.selected_type {
                let active_type = plan.unique_types()[i].clone();
                move_to(navigation, NavigationList::Create(active_type))
            } else {
                (navigation.clone(), Effect::NoOp)
            }
        }
        Key::Char('d') => {
            if let Some(i) = navigation.selected_type {
                let active_type = plan.unique_types()[i].clone();
                move_to(navigation, NavigationList::Delete(active_type))
            } else {
                (navigation.clone(), Effect::NoOp)
            }
        }

        // begin confirming an operation
        Key::Char('m') => {
            if let (Some(from), Some(to)) = (
                selected_resource(
                    plan,
                    TerraformAction::Delete,
                    navigation.selected_type,
                    navigation.selected_delete,
                    operations,
                ),
                selected_resource(
                    plan,
                    TerraformAction::Create,
                    navigation.selected_type,
                    navigation.selected_create,
                    operations,
                ),
            ) {
                let operation = Operation::Move {
                    from: from.address,
                    to: to.address,
                };
                (navigation.clone(), Effect::SeekConfirmation(operation))
            } else {
                (navigation.clone(), Effect::NoOp)
            }
        }
        Key::Char('r') => {
            if let Some(resource) = selected_resource(
                plan,
                TerraformAction::Delete,
                navigation.selected_type,
                navigation.selected_delete,
                operations,
            ) {
                let operation = Operation::Remove(resource.address);
                (navigation.clone(), Effect::SeekConfirmation(operation))
            } else {
                (navigation.clone(), Effect::NoOp)
            }
        }
        Key::Char('i') => {
            if let Some(resource) = selected_resource(
                plan,
                TerraformAction::Create,
                navigation.selected_type,
                navigation.selected_create,
                operations,
            ) {
                let operation = Operation::Import {
                    address: resource.address,
                    identifier: "".to_string(),
                };
                (navigation.clone(), Effect::SeekConfirmation(operation))
            } else {
                (navigation.clone(), Effect::NoOp)
            }
        }

        // unstage an operation
        Key::Delete => {
            if let (NavigationList::StagedOperations, Some(index)) =
                (&navigation.active_list, &navigation.selected_operation)
            {
                let nav = Navigation {
                    selected_operation: None,
                    ..navigation.clone()
                };
                (nav, Effect::UnstageOperation(*index))
            } else {
                (navigation.clone(), Effect::NoOp)
            }
        }

        _ => (navigation.clone(), Effect::NoOp),
    }
}

fn scroll_next(
    plan: &TerraformPlan,
    navigation: &Navigation,
    operations: &[Operation],
) -> (Navigation, Effect) {
    match &navigation.active_list {
        NavigationList::Types => {
            let nav = Navigation {
                selected_type: cycle_next(plan.unique_types().len(), &navigation.selected_type),
                selected_delete: None,
                selected_create: None,
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::Create(r#type) => {
            let list_length =
                unstaged_resources_of_type(r#type, &plan.pending_creation, operations).len();
            let nav = Navigation {
                selected_create: cycle_next(list_length, &navigation.selected_create),
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::Delete(r#type) => {
            let list_length =
                unstaged_resources_of_type(r#type, &plan.pending_deletion, operations).len();
            let nav = Navigation {
                selected_delete: cycle_next(list_length, &navigation.selected_delete),
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::StagedOperations => {
            let nav = Navigation {
                selected_operation: cycle_next(operations.len(), &navigation.selected_operation),
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
    }
}

fn scroll_previous(
    plan: &TerraformPlan,
    navigation: &Navigation,
    operations: &[Operation],
) -> (Navigation, Effect) {
    match &navigation.active_list {
        NavigationList::Types => {
            let nav = Navigation {
                selected_type: cycle_previous(plan.unique_types().len(), &navigation.selected_type),
                selected_delete: None,
                selected_create: None,
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::Create(r#type) => {
            let list_length =
                unstaged_resources_of_type(r#type, &plan.pending_creation, operations).len();
            let nav = Navigation {
                selected_create: cycle_previous(list_length, &navigation.selected_create),
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::Delete(r#type) => {
            let list_length =
                unstaged_resources_of_type(r#type, &plan.pending_deletion, operations).len();
            let nav = Navigation {
                selected_delete: cycle_previous(list_length, &navigation.selected_delete),
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::StagedOperations => {
            let nav = Navigation {
                selected_operation: cycle_previous(
                    operations.len(),
                    &navigation.selected_operation,
                ),
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
    }
}

fn deselect_list_item(navigation: &Navigation) -> (Navigation, Effect) {
    match navigation.active_list {
        NavigationList::Create(_) => {
            let nav = Navigation {
                selected_create: None,
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::Delete(_) => {
            let nav = Navigation {
                selected_delete: None,
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::StagedOperations => {
            let nav = Navigation {
                selected_operation: None,
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
        NavigationList::Types => {
            let nav = Navigation {
                selected_create: None,
                selected_delete: None,
                selected_type: None,
                ..navigation.clone()
            };
            (nav, Effect::NoOp)
        }
    }
}

fn move_to(navigation: &Navigation, list: NavigationList) -> (Navigation, Effect) {
    let nav = Navigation {
        active_list: list,
        ..navigation.clone()
    };
    (nav, Effect::NoOp)
}

fn keypress_while_confirming(
    navigation: &Navigation,
    operation: &Operation,
    key: Key,
) -> (Navigation, Effect) {
    if let Key::Delete = key {
        return (navigation.clone(), Effect::CloseConfirmationModal);
    }
    match operation {
        Operation::Import {
            address,
            identifier,
        } => keypress_while_confirming_import(navigation, address, identifier, key),
        Operation::Remove(_) => keypress_while_confirming_remove(navigation, operation, key),
        Operation::Move { .. } => keypress_while_confirming_move(navigation, operation, key),
    }
}

fn keypress_while_confirming_move(
    navigation: &Navigation,
    operation: &Operation,
    key: Key,
) -> (Navigation, Effect) {
    match key {
        Key::Char('\n') => {
            let nav = Navigation {
                selected_create: None,
                selected_delete: None,
                ..navigation.clone()
            };
            (nav, Effect::StageOperation(operation.clone()))
        }
        _ => (navigation.clone(), Effect::NoOp),
    }
}

fn keypress_while_confirming_remove(
    navigation: &Navigation,
    operation: &Operation,
    key: Key,
) -> (Navigation, Effect) {
    match key {
        Key::Char('\n') => {
            let nav = Navigation {
                selected_delete: None,
                ..navigation.clone()
            };
            (nav, Effect::StageOperation(operation.clone()))
        }
        _ => (navigation.clone(), Effect::NoOp),
    }
}

fn keypress_while_confirming_import(
    navigation: &Navigation,
    address: &str,
    identifier: &str,
    key: Key,
) -> (Navigation, Effect) {
    match key {
        Key::Char('\n') => (
            Navigation {
                selected_create: None,
                ..navigation.clone()
            },
            Effect::StageOperation(Operation::Import {
                address: address.to_string(),
                identifier: identifier.to_string(),
            }),
        ),
        Key::Backspace => match identifier.len() {
            0 => (navigation.clone(), Effect::NoOp),
            _ => {
                let mut updated_identifier = identifier.to_string();
                updated_identifier.pop();
                let updated_operation = Operation::Import {
                    address: address.to_string(),
                    identifier: updated_identifier,
                };
                (
                    navigation.clone(),
                    Effect::SeekConfirmation(updated_operation),
                )
            }
        },
        Key::Char(c) => {
            let mut updated_identifier = identifier.to_string();
            updated_identifier.push(c);
            let updated_operation = Operation::Import {
                address: address.to_string(),
                identifier: updated_identifier,
            };
            (
                navigation.clone(),
                Effect::SeekConfirmation(updated_operation),
            )
        }
        _ => (navigation.clone(), Effect::NoOp),
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

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operation::Move { from, to } => write!(f, "terraform state mv {} {}", from, to),
            Operation::Import {
                address,
                identifier,
            } => {
                write!(f, "terraform import {} {}", address, identifier)
            }
            Operation::Remove(address) => write!(f, "terraform state rm {}", address),
        }
    }
}

impl Operation {
    fn addresses(&self) -> Vec<String> {
        match self {
            Operation::Remove(address) => vec![address.clone()],
            Operation::Move { from, to } => vec![from.clone(), to.clone()],
            Operation::Import { address, .. } => vec![address.clone()],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use serde_json::json;

    struct FailClient {}

    impl Terraform for FailClient {
        fn show_plan(&self, _: &str) -> Result<TerraformPlan, Box<dyn Error>> {
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
    fn exit_effect_puts_model_in_exiting_action_state() {
        let mut model = Model::new();
        model.accept(Effect::Exit);
        assert_eq!(ActionState::Exiting, model.action_state);
    }

    #[test]
    fn open_help_modal_effect_puts_model_in_seeking_help_state() {
        let mut model = Model::new();
        model.accept(Effect::OpenHelpModal);
        assert_eq!(
            ActionState::SeekingHelp(Box::new(ActionState::Navigating)),
            model.action_state
        );
    }

    #[test]
    fn close_help_modal_effect_restores_model_to_previous_state() {
        let mut model = Model::new();
        model.action_state = ActionState::SeekingHelp(Box::new(ActionState::Navigating));

        model.accept(Effect::CloseHelpModal(ActionState::Navigating));

        assert_eq!(ActionState::Navigating, model.action_state);

        let operation = Operation::Remove("address".to_string());
        model.action_state =
            ActionState::SeekingHelp(Box::new(ActionState::Confirming(operation.clone())));

        model.accept(Effect::CloseHelpModal(ActionState::Confirming(
            operation.clone(),
        )));

        assert_eq!(
            ActionState::Confirming(operation.clone()),
            model.action_state
        );
    }

    #[test]
    fn seek_confirmation_effect_puts_model_in_confirming_state() {
        let mut model = Model::new();
        model.accept(Effect::SeekConfirmation(Operation::Remove(
            "address".to_string(),
        )));
        assert_eq!(
            ActionState::Confirming(Operation::Remove("address".to_string())),
            model.action_state
        );
    }

    #[test]
    fn close_confirmation_modal_effect_puts_model_in_navigating_state() {
        let mut model = Model::new();
        model.action_state = ActionState::Exiting;
        model.accept(Effect::CloseConfirmationModal);

        assert_eq!(ActionState::Navigating, model.action_state);
    }

    #[test]
    fn accepting_a_stage_operation_effect_adds_the_operation_to_the_model() {
        let mut model = Model::new();
        let operation = Operation::Move {
            from: "from".to_string(),
            to: "to".to_string(),
        };
        let stage_operation_effect = Effect::StageOperation(operation.clone());

        model.accept(stage_operation_effect);

        assert_eq!(1, model.staged_operations.len());
        assert_eq!(operation, model.staged_operations[0]);
    }

    #[test]
    fn accepting_a_stage_operation_effect_puts_model_in_navigating_state() {
        let mut model = Model::new();
        let operation = Operation::Move {
            from: "from".to_string(),
            to: "to".to_string(),
        };
        model.action_state = ActionState::Confirming(operation.clone());
        let stage_operation_effect = Effect::StageOperation(operation.clone());

        model.accept(stage_operation_effect);

        assert_eq!(ActionState::Navigating, model.action_state);
    }

    #[test]
    fn accepting_an_unstage_operation_effect_removes_operation_from_model() {
        let operations = vec![
            Operation::Remove("a".to_string()),
            Operation::Remove("b".to_string()),
            Operation::Remove("c".to_string()),
        ];
        let mut model = Model::new();
        model.staged_operations = operations;

        model.accept(Effect::UnstageOperation(1));

        assert_eq!(
            vec![
                Operation::Remove("a".to_string()),
                Operation::Remove("c".to_string()),
            ],
            model.staged_operations
        );
    }

    #[test]
    fn no_op_effect_does_not_alter_model() {
        let original_model = Model::new();
        let mut unaltered_model = original_model.clone();

        unaltered_model.accept(Effect::NoOp);

        assert_eq!(original_model, unaltered_model);
    }

    #[test]
    fn esc_sends_exit_effect() {
        let (_, effect) = keypress(&simple_plan(1), &Model::new(), Key::Esc);
        assert_eq!(Effect::Exit, effect);
    }

    #[test]
    fn q_sends_exit_effect() {
        let (_, effect) = keypress(&simple_plan(1), &Model::new(), Key::Char('q'));
        assert_eq!(Effect::Exit, effect);
    }

    #[test]
    fn cannot_cycle_into_an_empty_list() {
        assert!(cycle_next(0, &None).is_none());
        assert!(cycle_previous(0, &None).is_none());
    }

    #[test]
    fn while_confirming_remove_pressing_delete_closes_modal() {
        let plan = simple_plan(1);
        let action_state = ActionState::Confirming(Operation::Remove("address".to_string()));
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Delete);

        assert_eq!(navigation, nav);
        assert_eq!(Effect::CloseConfirmationModal, effect);
    }

    #[test]
    fn while_confirming_remove_pressing_return_stages_operation_and_deselects_delete_resource() {
        let plan = simple_plan(1);
        let operation = Operation::Remove("address".to_string());
        let action_state = ActionState::Confirming(operation.clone());
        let mut navigation = Navigation::default();
        navigation.selected_delete = Some(0);
        let mut model = Model::new();
        model.action_state = action_state;
        model.navigation = navigation.clone();

        let (nav, effect) = keypress(&plan, &model, Key::Char('\n'));

        let expected_nav = Navigation {
            selected_delete: None,
            ..navigation
        };
        assert_eq!(expected_nav, nav);
        assert_eq!(Effect::StageOperation(operation.clone()), effect);
    }

    #[test]
    fn while_confirming_remove_pressing_other_characters_does_nothing() {
        let plan = simple_plan(1);
        let action_state = ActionState::Confirming(Operation::Remove("address".to_string()));
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Left);

        assert_eq!(navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_confirming_move_pressing_delete_closes_modal() {
        let plan = simple_plan(1);
        let action_state = ActionState::Confirming(Operation::Move {
            from: "from".to_string(),
            to: "to".to_string(),
        });
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Delete);

        assert_eq!(navigation, nav);
        assert_eq!(Effect::CloseConfirmationModal, effect);
    }

    #[test]
    fn while_confirming_move_pressing_return_stages_operation_and_deselects_resources() {
        let plan = simple_plan(1);
        let operation = Operation::Move {
            from: "from".to_string(),
            to: "to".to_string(),
        };
        let action_state = ActionState::Confirming(operation.clone());
        let mut navigation = Navigation::default();
        navigation.selected_create = Some(0);
        navigation.selected_delete = Some(0);
        let mut model = Model::new();
        model.action_state = action_state;
        model.navigation = navigation.clone();

        let (nav, effect) = keypress(&plan, &model, Key::Char('\n'));

        let expected_nav = Navigation {
            selected_create: None,
            selected_delete: None,
            ..navigation
        };
        assert_eq!(expected_nav, nav);
        assert_eq!(Effect::StageOperation(operation.clone()), effect);
    }

    #[test]
    fn while_confirming_move_pressing_other_characters_does_nothing() {
        let plan = simple_plan(1);
        let action_state = ActionState::Confirming(Operation::Move {
            from: "from".to_string(),
            to: "to".to_string(),
        });
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Left);

        assert_eq!(navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_confirming_import_pressing_return_stages_operation_and_deselects_create_resource() {
        let plan = simple_plan(1);
        let operation = Operation::Import {
            address: "address".to_string(),
            identifier: "identifier".to_string(),
        };
        let action_state = ActionState::Confirming(operation.clone());
        let mut navigation = Navigation::default();
        navigation.selected_create = Some(0);
        let mut model = Model::new();
        model.action_state = action_state;
        model.navigation = navigation.clone();

        let (nav, effect) = keypress(&plan, &model, Key::Char('\n'));

        let expected_nav = Navigation {
            selected_create: None,
            ..navigation
        };
        assert_eq!(expected_nav, nav);
        assert_eq!(Effect::StageOperation(operation.clone()), effect);
    }

    #[test]
    fn while_confirming_import_pressing_backspace_deletes_from_identifier_safely() {
        let plan = simple_plan(1);
        let operation = Operation::Import {
            address: "address".to_string(),
            identifier: "i".to_string(),
        };
        let action_state = ActionState::Confirming(operation);
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Backspace);

        assert_eq!(navigation.clone(), nav);
        assert_eq!(
            Effect::SeekConfirmation(Operation::Import {
                address: "address".to_string(),
                identifier: "".to_string(),
            }),
            effect
        );

        model.accept(effect);

        let (nav, effect) = keypress(&plan, &model, Key::Backspace);

        assert_eq!(navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_confirming_import_pressing_delete_closes_modal() {
        let plan = simple_plan(1);
        let operation = Operation::Import {
            address: "address".to_string(),
            identifier: "".to_string(),
        };
        let action_state = ActionState::Confirming(operation);
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Delete);

        assert_eq!(navigation, nav);
        assert_eq!(Effect::CloseConfirmationModal, effect);
    }

    #[test]
    fn while_confirming_import_pressing_a_character_appends_to_identifier() {
        let plan = simple_plan(1);
        let action_state = ActionState::Confirming(Operation::Import {
            address: "address".to_string(),
            identifier: "".to_string(),
        });
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Char('i'));

        assert_eq!(navigation, nav);
        assert_eq!(
            Effect::SeekConfirmation(Operation::Import {
                address: "address".to_string(),
                identifier: "i".to_string(),
            }),
            effect
        );
    }

    #[test]
    fn while_confirming_import_pressing_other_keys_does_nothing() {
        let plan = simple_plan(1);
        let action_state = ActionState::Confirming(Operation::Import {
            address: "address".to_string(),
            identifier: "id".to_string(),
        });
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.action_state = action_state;

        let (nav, effect) = keypress(&plan, &model, Key::Left);

        assert_eq!(navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_i_seeks_confirmation_of_import_if_a_create_is_selected() {
        let plan = simple_plan(1);
        let mut navigation = Navigation::default();
        navigation.selected_type = Some(0);
        navigation.selected_create = Some(0);
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav, effect) = keypress(&plan, &model, Key::Char('i'));

        assert_eq!(model.navigation, nav);
        assert_eq!(
            Effect::SeekConfirmation(Operation::Import {
                address: "0.0".to_string(),
                identifier: "".to_string(),
            }),
            effect
        );
    }

    #[test]
    fn while_navigating_pressing_i_has_no_effect_if_a_create_is_not_selected() {
        let plan = simple_plan(1);
        let model = Model::new();

        let (nav, effect) = keypress(&plan, &model, Key::Char('i'));

        assert_eq!(model.navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_r_seeks_confirmation_of_removal_if_a_delete_is_selected() {
        let plan = simple_plan(1);
        let mut navigation = Navigation::default();
        navigation.selected_type = Some(0);
        navigation.selected_delete = Some(0);
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav, effect) = keypress(&plan, &model, Key::Char('r'));

        assert_eq!(model.navigation, nav);
        assert_eq!(
            Effect::SeekConfirmation(Operation::Remove("0.0".to_string())),
            effect
        );
    }

    #[test]
    fn while_navigating_pressing_r_has_no_effect_if_a_delete_is_not_selected() {
        let plan = simple_plan(1);
        let model = Model::new();

        let (nav, effect) = keypress(&plan, &model, Key::Char('r'));

        assert_eq!(model.navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_m_seeks_confirmation_of_move_if_create_and_delete_are_selected() {
        let plan = simple_plan(1);
        let mut navigation = Navigation::default();
        navigation.selected_type = Some(0);
        navigation.selected_delete = Some(0);
        navigation.selected_create = Some(0);
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav, effect) = keypress(&plan, &model, Key::Char('m'));

        assert_eq!(model.navigation, nav);
        assert_eq!(
            Effect::SeekConfirmation(Operation::Move {
                from: "0.0".to_string(),
                to: "0.0".to_string(),
            }),
            effect
        );
    }

    #[test]
    fn while_navigating_pressing_m_has_no_effect_if_either_create_or_delete_is_not_selected() {
        let plan = simple_plan(1);
        let model = Model::new();

        let (nav, effect) = keypress(&plan, &model, Key::Char('m'));

        assert_eq!(model.navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_t_moves_to_types_list() {
        let plan = simple_plan(1);
        let mut navigation = Navigation::default();
        navigation.active_list = NavigationList::StagedOperations;
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav, effect) = keypress(&plan, &model, Key::Char('t'));

        assert_eq!(
            Navigation {
                active_list: NavigationList::Types,
                ..model.navigation.clone()
            },
            nav
        );
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_c_moves_to_create_list_if_type_is_selected() {
        let plan = simple_plan(1);
        let mut navigation = Navigation::default();
        navigation.selected_type = Some(0);
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav, effect) = keypress(&plan, &model, Key::Char('c'));

        assert_eq!(
            Navigation {
                active_list: NavigationList::Create("0".to_string()),
                ..model.navigation.clone()
            },
            nav
        );
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_c_does_nothing_if_no_type_is_selected() {
        let plan = simple_plan(1);
        let model = Model::new();

        let (nav, effect) = keypress(&plan, &model, Key::Char('c'));

        assert_eq!(model.navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_d_moves_to_delete_list_if_type_is_selected() {
        let plan = simple_plan(1);
        let mut navigation = Navigation::default();
        navigation.selected_type = Some(0);
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav, effect) = keypress(&plan, &model, Key::Char('d'));

        assert_eq!(
            Navigation {
                active_list: NavigationList::Delete("0".to_string()),
                ..model.navigation.clone()
            },
            nav
        );
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_d_does_nothing_if_no_type_is_selected() {
        let plan = simple_plan(1);
        let model = Model::new();

        let (nav, effect) = keypress(&plan, &model, Key::Char('d'));

        assert_eq!(model.navigation, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_s_moves_to_staged_operations_list() {
        let plan = simple_plan(1);
        let model = Model::new();

        let (nav, effect) = keypress(&plan, &model, Key::Char('s'));

        assert_eq!(
            Navigation {
                active_list: NavigationList::StagedOperations,
                ..model.navigation.clone()
            },
            nav
        );
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_in_types_list_user_can_scroll_forward() {
        let plan = simple_plan(3);
        let model = Model::new();

        let (nav0, effect) = keypress(&plan, &model, Key::Char('j'));

        assert_eq!(
            Navigation {
                selected_type: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );
        assert_eq!(Effect::NoOp, effect);

        let mut model = Model::new();
        model.navigation = nav0;

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('n'));

        assert_eq!(
            Navigation {
                selected_type: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;

        let (nav2, _) = keypress(&plan, &model, Key::Down);

        assert_eq!(
            Navigation {
                selected_type: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );

        let mut model = Model::new();
        model.navigation = nav2;

        let (nav_reset, _) = keypress(&plan, &model, Key::Down);

        assert_eq!(
            Navigation {
                selected_type: Some(0),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_in_types_list_user_can_scroll_backward() {
        let plan = simple_plan(3);
        let model = Model::new();

        let (nav2, effect) = keypress(&plan, &model, Key::Char('k'));

        assert_eq!(
            Navigation {
                selected_type: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );
        assert_eq!(Effect::NoOp, effect);

        let mut model = Model::new();
        model.navigation = nav2;

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('p'));

        assert_eq!(
            Navigation {
                selected_type: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;

        let (nav0, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_type: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );

        let mut model = Model::new();
        model.navigation = nav0;

        let (nav_reset, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_type: Some(2),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_in_types_list_scrolling_deselects_resources() {
        let plan = simple_plan(2);
        let mut navigation = Navigation::default();
        navigation.selected_type = Some(0);
        navigation.selected_delete = Some(0);
        navigation.selected_create = Some(0);
        let mut model = Model::new();
        model.navigation = navigation.clone();

        let (nav_prev, _) = keypress(&plan, &model, Key::Up);
        let (nav_next, _) = keypress(&plan, &model, Key::Down);

        let expected_navigation = Navigation {
            selected_type: Some(1),
            selected_delete: None,
            selected_create: None,
            ..navigation.clone()
        };
        assert_eq!(expected_navigation, nav_prev);
        assert_eq!(expected_navigation, nav_next);
    }

    #[test]
    fn while_navigating_in_create_list_user_can_scroll_forward() {
        let mut plan = simple_plan(1);
        // add two more resources pending creation, for a total of 3
        plan.pending_creation.push(TerraformResource {
            address: "0.1".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });
        plan.pending_creation.push(TerraformResource {
            address: "0.2".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });
        let mut navigation = Navigation::default();
        navigation.active_list = NavigationList::Create("0".to_string());
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav0, _) = keypress(&plan, &model, Key::Char('j'));

        assert_eq!(
            Navigation {
                selected_create: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );

        let mut model = Model::new();
        model.navigation = nav0;

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('n'));

        assert_eq!(
            Navigation {
                selected_create: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;

        let (nav2, _) = keypress(&plan, &model, Key::Down);

        assert_eq!(
            Navigation {
                selected_create: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );

        let mut model = Model::new();
        model.navigation = nav2;

        let (nav_reset, _) = keypress(&plan, &model, Key::Down);

        assert_eq!(
            Navigation {
                selected_create: Some(0),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_in_create_list_user_can_scroll_backward() {
        let mut plan = simple_plan(1);
        // add two more resources pending creation, for a total of 3
        plan.pending_creation.push(TerraformResource {
            address: "0.1".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });
        plan.pending_creation.push(TerraformResource {
            address: "0.2".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });
        let mut navigation = Navigation::default();
        navigation.active_list = NavigationList::Create("0".to_string());
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav2, _) = keypress(&plan, &model, Key::Char('k'));

        assert_eq!(
            Navigation {
                selected_create: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );

        let mut model = Model::new();
        model.navigation = nav2;

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('p'));

        assert_eq!(
            Navigation {
                selected_create: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;

        let (nav0, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_create: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );

        let mut model = Model::new();
        model.navigation = nav0;

        let (nav_reset, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_create: Some(2),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_in_delete_list_user_can_scroll_forward() {
        let mut plan = simple_plan(1);
        // add two more resources pending deletion, for a total of 3
        plan.pending_deletion.push(TerraformResource {
            address: "0.1".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });
        plan.pending_deletion.push(TerraformResource {
            address: "0.2".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });

        let mut navigation = Navigation::default();
        navigation.active_list = NavigationList::Delete("0".to_string());
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav0, _) = keypress(&plan, &model, Key::Char('j'));

        assert_eq!(
            Navigation {
                selected_delete: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );

        let mut model = Model::new();
        model.navigation = nav0;

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('n'));

        assert_eq!(
            Navigation {
                selected_delete: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;

        let (nav2, _) = keypress(&plan, &model, Key::Down);

        assert_eq!(
            Navigation {
                selected_delete: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );

        let mut model = Model::new();
        model.navigation = nav2;

        let (nav_reset, _) = keypress(&plan, &model, Key::Down);

        assert_eq!(
            Navigation {
                selected_delete: Some(0),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_in_delete_list_user_can_scroll_backward() {
        let mut plan = simple_plan(1);
        // add two more resources pending deletion, for a total of 3
        plan.pending_deletion.push(TerraformResource {
            address: "0.1".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });
        plan.pending_deletion.push(TerraformResource {
            address: "0.2".to_string(),
            r#type: "0".to_string(),
            preview: json!({}),
        });

        let mut navigation = Navigation::default();
        navigation.active_list = NavigationList::Delete("0".to_string());
        let mut model = Model::new();
        model.navigation = navigation;

        let (nav2, _) = keypress(&plan, &model, Key::Char('k'));

        assert_eq!(
            Navigation {
                selected_delete: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );

        let mut model = Model::new();
        model.navigation = nav2;

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('p'));

        assert_eq!(
            Navigation {
                selected_delete: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;

        let (nav0, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_delete: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );

        let mut model = Model::new();
        model.navigation = nav0;

        let (nav_reset, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_delete: Some(2),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_in_staged_operations_list_user_can_scroll_forward() {
        let plan = simple_plan(1);
        let staged_operations = vec![
            Operation::Remove("a".to_string()),
            Operation::Remove("b".to_string()),
            Operation::Remove("c".to_string()),
        ];
        let mut model = Model::new();
        model.navigation = Navigation {
            active_list: NavigationList::StagedOperations,
            ..model.navigation
        };
        model.staged_operations = staged_operations.clone();
        let (nav0, effect) = keypress(&plan, &model, Key::Char('j'));

        assert_eq!(
            Navigation {
                selected_operation: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );
        assert_eq!(Effect::NoOp, effect);

        let mut model = Model::new();
        model.navigation = nav0;
        model.staged_operations = staged_operations.clone();

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('n'));

        assert_eq!(
            Navigation {
                selected_operation: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;
        model.staged_operations = staged_operations.clone();

        let (nav2, _) = keypress(&plan, &model, Key::Ctrl('n'));

        assert_eq!(
            Navigation {
                selected_operation: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );

        let mut model = Model::new();
        model.navigation = nav2;
        model.staged_operations = staged_operations.clone();

        let (nav_reset, _) = keypress(&plan, &model, Key::Ctrl('n'));

        assert_eq!(
            Navigation {
                selected_operation: Some(0),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_in_staged_operations_list_user_can_scroll_backward() {
        let plan = simple_plan(1);
        let staged_operations = vec![
            Operation::Remove("a".to_string()),
            Operation::Remove("b".to_string()),
            Operation::Remove("c".to_string()),
        ];
        let mut model = Model::new();
        model.navigation = Navigation {
            active_list: NavigationList::StagedOperations,
            ..model.navigation
        };
        model.staged_operations = staged_operations.clone();

        let (nav2, effect) = keypress(&plan, &model, Key::Char('k'));

        assert_eq!(
            Navigation {
                selected_operation: Some(2),
                ..model.navigation.clone()
            },
            nav2
        );
        assert_eq!(Effect::NoOp, effect);

        let mut model = Model::new();
        model.navigation = nav2;
        model.staged_operations = staged_operations.clone();

        let (nav1, _) = keypress(&plan, &model, Key::Ctrl('p'));

        assert_eq!(
            Navigation {
                selected_operation: Some(1),
                ..model.navigation.clone()
            },
            nav1
        );

        let mut model = Model::new();
        model.navigation = nav1;
        model.staged_operations = staged_operations.clone();

        let (nav0, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_operation: Some(0),
                ..model.navigation.clone()
            },
            nav0
        );

        let mut model = Model::new();
        model.navigation = nav0;
        model.staged_operations = staged_operations.clone();

        let (nav_reset, _) = keypress(&plan, &model, Key::Up);

        assert_eq!(
            Navigation {
                selected_operation: Some(2),
                ..model.navigation.clone()
            },
            nav_reset
        );
    }

    #[test]
    fn while_navigating_create_delete_or_staged_operations_lists_pressing_backspace_deselects_list_item(
    ) {
        let plan = simple_plan(1);
        let mut fully_selected_nav = Navigation {
            selected_type: Some(0),
            selected_create: Some(0),
            selected_delete: Some(0),
            selected_operation: Some(0),
            active_list: NavigationList::Create("0".to_string()),
        };
        let mut model = Model::new();
        model.navigation = fully_selected_nav.clone();

        let (nav_without_create, effect) = keypress(&plan, &model, Key::Backspace);

        assert_eq!(
            Navigation {
                selected_create: None,
                ..fully_selected_nav
            },
            nav_without_create
        );
        assert_eq!(Effect::NoOp, effect);

        fully_selected_nav.active_list = NavigationList::Delete("0".to_string());
        model.navigation = fully_selected_nav.clone();

        let (nav_without_delete, effect) = keypress(&plan, &model, Key::Backspace);

        assert_eq!(
            Navigation {
                selected_delete: None,
                ..fully_selected_nav
            },
            nav_without_delete
        );
        assert_eq!(Effect::NoOp, effect);

        fully_selected_nav.active_list = NavigationList::StagedOperations;
        model.navigation = fully_selected_nav.clone();

        let (nav_without_operation, effect) = keypress(&plan, &model, Key::Backspace);

        assert_eq!(
            Navigation {
                selected_operation: None,
                ..fully_selected_nav
            },
            nav_without_operation
        );
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_types_list_pressing_backspace_deselects_type_and_resources() {
        let plan = simple_plan(1);
        let fully_selected_nav = Navigation {
            selected_type: Some(0),
            selected_create: Some(0),
            selected_delete: Some(0),
            selected_operation: Some(0),
            active_list: NavigationList::Types,
        };
        let mut model = Model::new();
        model.navigation = fully_selected_nav.clone();

        let (nav, effect) = keypress(&plan, &model, Key::Backspace);

        assert_eq!(
            Navigation {
                selected_type: None,
                selected_create: None,
                selected_delete: None,
                ..fully_selected_nav.clone()
            },
            nav
        );
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_staged_operations_with_an_operation_selected_pressing_delete_unstages_an_operation(
    ) {
        let plan = simple_plan(1);
        let mut navigation = Navigation::default();
        navigation.active_list = NavigationList::StagedOperations;
        navigation.selected_operation = Some(1);
        let mut model = Model::new();
        model.navigation = navigation.clone();

        let (nav, effect) = keypress(&plan, &model, Key::Delete);

        assert_eq!(
            Navigation {
                selected_operation: None,
                ..navigation
            },
            nav
        );
        assert_eq!(Effect::UnstageOperation(1), effect);
    }

    #[test]
    fn while_navigating_pressing_delete_has_no_effect_when_browsing_other_lists_or_no_operation_is_selected(
    ) {
        let plan = simple_plan(1);
        let navigation = Navigation::default();
        let mut model = Model::new();
        model.navigation = navigation.clone();

        let (nav, effect) = keypress(&plan, &model, Key::Delete);

        assert_eq!(navigation.clone(), nav);
        assert_eq!(Effect::NoOp, effect);

        let correct_list_no_selection_nav = Navigation {
            active_list: NavigationList::StagedOperations,
            ..navigation.clone()
        };
        model.navigation = correct_list_no_selection_nav.clone();
        let (nav, effect) = keypress(&plan, &model, Key::Delete);

        assert_eq!(correct_list_no_selection_nav, nav);
        assert_eq!(Effect::NoOp, effect);

        let selection_but_incorrect_list_nav = Navigation {
            selected_operation: Some(0),
            ..navigation.clone()
        };
        model.navigation = selection_but_incorrect_list_nav.clone();
        let (nav, effect) = keypress(&plan, &model, Key::Delete);

        assert_eq!(selection_but_incorrect_list_nav, nav);
        assert_eq!(Effect::NoOp, effect);
    }

    #[test]
    fn while_navigating_pressing_h_or_questionmark_toggles_help_modal() {
        let plan = simple_plan(1);
        let mut model = Model::new();

        let (_, effect_h) = keypress(&plan, &model, Key::Char('h'));
        let (_, effect_question) = keypress(&plan, &model, Key::Char('?'));

        assert_eq!(Effect::OpenHelpModal, effect_h);
        assert_eq!(Effect::OpenHelpModal, effect_question);

        model.action_state = ActionState::SeekingHelp(Box::new(ActionState::Navigating));

        let (_, effect_h) = keypress(&plan, &model, Key::Char('h'));
        let (_, effect_question) = keypress(&plan, &model, Key::Char('?'));

        assert_eq!(Effect::CloseHelpModal(ActionState::Navigating), effect_h);
        assert_eq!(
            Effect::CloseHelpModal(ActionState::Navigating),
            effect_question
        );
    }

    #[test]
    fn while_confirming_operation_pressing_h_or_questionmark_toggles_help_modal() {
        let plan = simple_plan(1);
        let operation = Operation::Remove("address".to_string());
        let mut model = Model::new();
        model.action_state = ActionState::Confirming(operation.clone());

        let (_, effect_h) = keypress(&plan, &model, Key::Char('h'));
        let (_, effect_question) = keypress(&plan, &model, Key::Char('?'));

        assert_eq!(Effect::OpenHelpModal, effect_h);
        assert_eq!(Effect::OpenHelpModal, effect_question);

        model.action_state =
            ActionState::SeekingHelp(Box::new(ActionState::Confirming(operation.clone())));

        let (_, effect_h) = keypress(&plan, &model, Key::Char('h'));
        let (_, effect_question) = keypress(&plan, &model, Key::Char('?'));

        assert_eq!(
            Effect::CloseHelpModal(ActionState::Confirming(operation.clone())),
            effect_h
        );
        assert_eq!(
            Effect::CloseHelpModal(ActionState::Confirming(operation.clone())),
            effect_question
        );
    }
}
