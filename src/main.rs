use std::process::Command;

use structopt::StructOpt;
use sandu::{Sandu, Terraform};

fn main() -> Result<(), String> {
    let sandu = Sandu::from_args();

    sandu::run(sandu, &TerraformCli {})
}

struct TerraformCli {}
impl Terraform for TerraformCli {
    fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, String> {
        let cmd = Command::new("terraform")
            .args(["show", "-json", &planfile])
            .output()
            .expect("Failed to execute terraform show -json with provided plan file");
        Ok(cmd.stdout)
    }
}
