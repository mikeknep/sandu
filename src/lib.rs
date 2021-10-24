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
    layout::{Constraint, Direction, Layout},
    style::{Color, Style},
    text::Span,
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
    let tfplan = serde_json::from_slice::<TfPlan>(&json_bytes)?;

    let model = Model::init(tfplan);

    let stdout = io::stdout().into_raw_mode()?;
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    let mut asi = termion::async_stdin().keys();
    terminal.clear()?;

    loop {
        draw(&mut terminal, &model)?;
        match get_msg(&mut asi) {
            Msg::Quit => {
                terminal.clear()?;
                return Ok(());
            }
            Msg::DoNothing => {}
        }
    }
}

fn draw<B>(terminal: &mut Terminal<B>, model: &Model) -> Result<(), Box<dyn Error>>
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

        let types = Block::default()
            .title(vec![Span::from("Types")])
            .border_style(Style::default().fg(Color::Gray))
            .borders(Borders::ALL);

        let destroying = Block::default()
            .title(vec![Span::styled(
                "Destroying",
                Style::default().fg(Color::Red),
            )])
            .border_style(Style::default().fg(Color::Gray))
            .borders(Borders::ALL);

        let creating = Block::default()
            .title(vec![Span::styled(
                "Creating",
                Style::default().fg(Color::Green),
            )])
            .border_style(Style::default().fg(Color::Gray))
            .borders(Borders::ALL);

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

        let creating_preview = Block::default()
            .title(vec![Span::styled(
                "Creating",
                Style::default().fg(Color::Green),
            )])
            .border_style(Style::default().fg(Color::Gray))
            .borders(Borders::ALL);

        let destroying_preview = Block::default()
            .title(vec![Span::styled(
                "Destroying",
                Style::default().fg(Color::Red),
            )])
            .border_style(Style::default().fg(Color::Gray))
            .borders(Borders::ALL);

        let staged = Block::default()
            .title(vec![Span::from("Staged")])
            .border_style(Style::default().fg(Color::Gray))
            .borders(Borders::ALL);

        let help = Block::default()
            .title(vec![Span::from("Help")])
            .border_style(Style::default().fg(Color::Gray))
            .borders(Borders::ALL);

        f.render_widget(types, operations_pane[0]);
        f.render_widget(destroying, operations_pane[1]);
        f.render_widget(creating, operations_pane[2]);
        f.render_widget(destroying_preview, resource_preview[0]);
        f.render_widget(creating_preview, resource_preview[1]);
        f.render_widget(staged, preview_pane[1]);
        f.render_widget(help, preview_pane[2]);
    })?;
    Ok(())
}

fn get_msg(asi: &mut termion::input::Keys<termion::AsyncReader>) -> Msg {
    match asi.next() {
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
    name: String,
    provider_name: String,
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
    staged_operations: Vec<String>,
    unstaged_resources: Vec<ChangingResource>,
}

impl Model {
    fn init(tfplan: TfPlan) -> Model {
        Model {
            staged_operations: vec![],
            unstaged_resources: tfplan.changing_resources,
        }
    }
}

enum Msg {
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
        fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, Box<dyn Error>> {
            Err(SanduError::new("Terraform failed!"))
        }
    }

    impl Filesystem for FailClient {
        fn file_exists(&self, path: &str) -> bool {
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
    fn all_changing_resources_are_unstaged_on_model_init() {
        let tfplan = TfPlan {
            changing_resources: vec![
                ChangingResource {
                    address: "module.original.random_pet.tapu".to_string(),
                    name: "tapu".to_string(),
                    provider_name: "registry.terraform.io/hashicorp/random".to_string(),
                    r#type: "random_pet".to_string(),
                    change: Change {
                        actions: vec!["delete".to_string()],
                        before: Some(json!({ "id": "careful-escargot", "separator" : "-" })),
                        after: None,
                    },
                },
                ChangingResource {
                    address: "module.changed.random_pet.tapu".to_string(),
                    name: "tapu".to_string(),
                    provider_name: "registry.terraform.io/hashicorp/random".to_string(),
                    r#type: "random_pet".to_string(),
                    change: Change {
                        actions: vec!["create".to_string()],
                        before: None,
                        after: Some(json!({ "separator" : "-" })),
                    },
                },
            ],
        };
        let model = Model::init(tfplan);

        assert_eq!(2, model.unstaged_resources.len());
    }
}
