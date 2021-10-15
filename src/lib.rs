use serde::Deserialize;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name="sandu", about="Interactive Terraform state surgery")]
pub struct Sandu {
    planfile: String,
}

pub fn run(sandu: Sandu, terraform: &dyn Terraform, filesystem: &dyn Filesystem) -> Result<(), String> {
    if !filesystem.file_exists(&sandu.planfile) {
        return Err("Provided file does not exist".to_string())
    }
    let json_bytes = terraform.show_plan(&sandu.planfile).expect("Invalid Terraform plan file provided");
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

    struct FailingTerraform {}
    impl Terraform for FailingTerraform {
        fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, String> {
            Err("Terraform failed!".to_string())
        }
    }

    struct EmptyFilesystem {}
    impl Filesystem for EmptyFilesystem {
        fn file_exists(&self, path: &str) -> bool { return false }
    }

    #[test]
    fn returns_error_when_planfile_does_not_exist() {
        let sandu = Sandu{ planfile: "does_not_exist".to_string() };

        let result = run(sandu, &FailingTerraform {}, &EmptyFilesystem {});
        assert_eq!(Err("Provided file does not exist".to_string()), result);
    }
}
