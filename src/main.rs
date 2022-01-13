use clap::Parser;
use std::error::Error;

use crate::real::{LocalFilesystem, TerraformCli};
use sandu::{Clients, Sandu};

mod real;

fn main() -> Result<(), Box<dyn Error>> {
    let sandu = Sandu::parse();
    let clients = Clients {
        filesystem: &LocalFilesystem {},
        terraform: &TerraformCli {},
    };

    sandu::run(sandu, clients)
}
