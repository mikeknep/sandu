use std::error::Error;
use structopt::StructOpt;

use crate::real::{LocalFilesystem, TerraformCli};
use sandu::{Clients, Sandu};

mod real;

fn main() -> Result<(), Box<dyn Error>> {
    let sandu = Sandu::from_args();
    let clients = Clients {
        filesystem: &LocalFilesystem {},
        terraform: &TerraformCli {},
    };

    sandu::run(sandu, clients)
}
