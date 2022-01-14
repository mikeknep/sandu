# Sandu

Interactive Terraform state surgery

## Purpose

Refactoring Terraform code can be tricky.
Renaming modules and reorganizing resources can lead to lengthy, complex plans that default to tearing down and recreating resources, which is typically unacceptable.
A "pure" refactor in Terraform should only change the _code representation_ of resources without having any effect on the actual managed resources themselves.

Terraform provides several commands to facilitate this,
such as [importing](https://www.terraform.io/cli/commands/import) existing resources into Terraform's control,
[removing](https://www.terraform.io/cli/commands/state/rm) resources from Terraform without destroying them,
and [moving](https://www.terraform.io/cli/commands/state/mv) resources within Terraform code without otherwise affecting them.

Sandu is designed to assist with this "state surgery."
Given a Terrafrom plan file, Sandu organizes the resources Terraform expects to create and destroy and provides a more pleasant interface for reviewing the plan and producing a list of operations to run.

## Using Sandu

### Prerequisites

- Sandu must be run in an initialized Terraform directory.
- Sandu takes one argument as input: a (binary) output file from planning Terraform (i.e. `terraform plan -out tfplan`).

### Navigating Sandu

#### At any time
- `h` | `?`: Toggle help menu
- `q` | `Esc`: Exit sandu

#### While browsing
- `t`: Move to Types list pane
- `s`: Move to Staged operations list pane
- `d`: Move to resources pending deletion list pane
- `c`: Move to resources pending creation list pane
- `UP arrow` | `k` | `ctrl-p`: Scroll to previous list item
- `DOWN arrow` | `j` | `ctrl-n`: Scroll to next list item
- `Backspace`: Deselect from the current list
- `i`: Stage an import operation (must have a resource pending creation selected)
- `r`: Stage a remove operation (must have a resource pending deletion selected)
- `m`: Stage a move operation (must have resources pending creation and deletion selected)
- `Delete`: Unstage an operation (must be in staged operations list with an operation selected)

#### While confirming an operation
- `Enter/Return`: Stage the operation
- `Delete`: Abort, return to browsing without staging the operation

(Note you can also freely type in a resource identifier while confirming an import operation.)


## Examples

The `example/` directory is a Terraform entrypoint.
You'll need to run `terraform init`, but there is existing local state checked in.
The `main.tf` file is **out of sync** with that state—specifically, the module has been renamed from `demo` to `renamed`.
The checked-in `tfplan` file is the result of running `terraform plan -out tfplan` in this exact state.

This is a perfect situation in which to use Sandu!

If you want to try out Sandu without installing both Rust and Terraform, you can run the example in a Docker container.
From the root of this repository, run:
```
docker build -t sandu-example -f example/Dockerfile .
docker run --rm -it sandu-example
```

## Developing Sandu

```
cargo test
cargo build
```
