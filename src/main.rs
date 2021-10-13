use structopt::StructOpt;
use sandu::Sandu;

fn main() {
    let sandu = Sandu::from_args();

    sandu::run(sandu);
}
