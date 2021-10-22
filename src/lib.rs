use serde::Deserialize;
use std::error::Error;
use std::fmt;
use std::io::{self, Read};
use structopt::StructOpt;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use tui::backend::{Backend, TermionBackend};
use tui::widgets::{Block, Borders};
use tui::Terminal;

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
struct SanduError {
    details: String,
}

impl SanduError {
    fn new(msg: &str) -> Box<SanduError> {
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
    terminal.draw(|rect| {
        let size = rect.size();
        let block = Block::default()
            .title(format!("{:?}", model.unstaged_resources[0].address)) // temporary, to demo presenting the model
            .borders(Borders::ALL);
        rect.render_widget(block, size);
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
    after: Option<serde_json::Map<String, serde_json::Value>>,
    before: Option<serde_json::Map<String, serde_json::Value>>,
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
}
