use std::error::Error;
use std::path::Path;
use std::process::Command;

use sandu::terraform;
use sandu::terraform::{Terraform, TerraformPlan};
use sandu::{Filesystem, SanduError};

pub struct TerraformCli {}
impl Terraform for TerraformCli {
    fn show_plan(&self, planfile: &str) -> Result<TerraformPlan, Box<dyn Error>> {
        let cmd = Command::new("terraform")
            .args(["show", "-json", planfile])
            .output()?;
        if cmd.status.success() {
            terraform::parse(&cmd.stdout)
        } else {
            Err(SanduError::new(
                "Could not run `terraform show -json` on provided file",
            ))
        }
    }
}

pub struct LocalFilesystem {}
impl Filesystem for LocalFilesystem {
    fn file_exists(&self, path: &str) -> bool {
        Path::new(&path).is_file()
    }
}
