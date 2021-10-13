use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name="sandu", about="Interactive Terraform state surgery")]
pub struct Sandu {
    planfile: String,
}

pub fn run(sandu: Sandu) {
    println!("Successfully called Sandu with {}", sandu.planfile)
}
