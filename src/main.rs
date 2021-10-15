use std::path::Path;
use std::process::Command;

use sandu::{Filesystem, Sandu, Terraform};
use structopt::StructOpt;

fn main() -> Result<(), String> {
    let sandu = Sandu::from_args();

    sandu::run(sandu, &TerraformCli {}, &LocalFilesystem {})
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

struct LocalFilesystem {}
impl Filesystem for LocalFilesystem {
    fn file_exists(&self, path: &str) -> bool {
        Path::new(&path).is_file()
    }
}
