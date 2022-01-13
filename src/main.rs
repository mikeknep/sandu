use std::error::Error;
use std::path::Path;
use structopt::StructOpt;

use crate::real::TerraformCli;
use sandu::{Clients, Filesystem, Sandu};

mod real;

fn main() -> Result<(), Box<dyn Error>> {
    let sandu = Sandu::from_args();
    let clients = Clients {
        filesystem: &LocalFilesystem {},
        terraform: &TerraformCli {},
    };

    sandu::run(sandu, clients)
}

struct LocalFilesystem {}
impl Filesystem for LocalFilesystem {
    fn file_exists(&self, path: &str) -> bool {
        Path::new(&path).is_file()
    }
}
