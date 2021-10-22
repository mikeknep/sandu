use std::error::Error;
use std::path::Path;
use std::process::Command;

use sandu::{Clients, Filesystem, Sandu, SanduError, Terraform};
use structopt::StructOpt;

fn main() -> Result<(), Box<dyn Error>> {
    let sandu = Sandu::from_args();
    let clients = Clients {
        filesystem: &LocalFilesystem {},
        terraform: &TerraformCli {},
    };

    sandu::run(sandu, clients)
}

struct TerraformCli {}
impl Terraform for TerraformCli {
    fn show_plan(&self, planfile: &str) -> Result<Vec<u8>, Box<dyn Error>> {
        let cmd = Command::new("terraform")
            .args(["show", "-json", planfile])
            .output()?;
        if cmd.status.success() {
            Ok(cmd.stdout)
        } else {
            Err(SanduError::new(
                "Could not run `terraform show -json` on provided file",
            ))
        }
    }
}

struct LocalFilesystem {}
impl Filesystem for LocalFilesystem {
    fn file_exists(&self, path: &str) -> bool {
        Path::new(&path).is_file()
    }
}
