# Sandu

Interactive state surgery

## Using Sandu

- Sandu must be run in an initialized Terraform directory.
- Sandu takes one argument as input: a (binary) output file from planning Terraform (i.e. `terraform plan -out tfplan`).

## Examples

The `example/` directory is a Terraform entrypoint.
You'll need to run `terraform init`, but there is existing local state checked in.
The `main.tf` file is **out of sync** with that state—specifically, the module has been renamed from `demo` to `renamed`.
The checked-in `tfplan` file is the result of running `terraform plan -out tfplan` in this exact state.

This is a perfect situation in which to use Sandu!

## Developing Sandu

```
cargo test
cargo build
```
