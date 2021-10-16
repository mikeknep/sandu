use serde::Deserialize;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "sandu", about = "Interactive Terraform state surgery")]
pub struct Sandu {
    planfile: String,
}

pub struct Clients<'a> {
    pub filesystem: &'a dyn Filesystem,
    pub terraform: &'a dyn Terraform,
}

pub fn run(sandu: Sandu, clients: Clients) -> Result<(), String> {
    if !clients.filesystem.file_exists(&sandu.planfile) {
        return Err("Provided file does not exist".to_string());
    }
    let json_bytes = clients
        .terraform
        .show_plan(&sandu.planfile)
        .expect("Invalid Terraform plan file provided");
    let tfplan = serde_json::from_slice::<TfPlan>(&json_bytes).unwrap();
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
    fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, String>;
}

pub trait Filesystem {
    fn file_exists(&self, path: &str) -> bool;
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    struct FailClient {}

    impl Terraform for FailClient {
        fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, String> {
            Err("Terraform failed!".to_string())
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
        assert_eq!(Err("Provided file does not exist".to_string()), result);
    }
}
