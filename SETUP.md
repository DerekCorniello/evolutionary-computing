# volutionary Computing - Setup Instructions

This repository contains assignments, a project, and a reusable utility library for the Evolutionary Computing course.

## Repository Structure

```
evolutionary-computing/
├─ dune-project           # Dune project file
├─ evolutionary_computing.opam
├─ util/                  # Reusable library
│  ├─ dune
│  ├─ *.ml               # OCaml source files
│  └─ *.mli              # Interface files (optional)
├─ assignments/           # Assignment executables
│  └─ hwX/
│     ├─ dune
│     └─ hwX.ml
│     ...
├─ project/               # Main project executable
│  ├─ dune
│  └─ main.ml
├─ .gitignore
└─ SETUP.md               # This file
```

* **util/**: Contains helper functions that can be used in assignments or the project
* **assignments/**: Each assignment has its own folder and executable
* **project/**: Final project code
* **dune-project**: Defines the Dune project configuration
* **evolutionary_computing.opam**: Package specification for dependency management

## Requirements

* OCaml 5.2.0 (recommended for this project)
* Dune 3.x or later
* OPAM (OCaml package manager)
* Optional: `utop` for interactive testing

## Installation

### Step 1: Install OPAM

**Linux (Debian/Ubuntu):**
```bash
sudo apt update
sudo apt install opam
```

**Linux (Arch):**
```bash
sudo pacman -S opam
```

**Linux (Fedora):**
```bash
sudo dnf install opam
```

**macOS:**
```bash
brew install opam
```

**Alternative (any Unix system):**
```bash
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```

### Step 2: Initialize OPAM and Install OCaml

```bash
# Initialize opam (only needed once)
opam init

# Set up the environment for current session
eval $(opam env)

# Create and switch to OCaml 5.2.0
opam switch create 5.2.0
eval $(opam env)

# Install required packages
opam install dune utop
```

### Step 3: Environment Setup

Add this line to your shell configuration file (`~/.bashrc`, `~/.zshrc`, etc.) to automatically set up the OCaml environment:

```bash
eval $(opam env)
```

Or run `eval $(opam env)` manually each time you open a new terminal.

## Build Instructions

From the root of the repository:

```bash
# Build all executables and libraries
dune build

# Clean build artifacts (if needed)
dune clean
```

This compiles the utility library, assignments, and project executables.

### Verify Installation

To ensure everything is working correctly, run the test installation:

```bash
dune exec assignments/test_install/test_install.exe
```

## Running Code

### Run Assignments

Each assignment executable is located under `assignments/<hwX>/`. For example:

```bash
# Run homework 1
dune exec assignments/hw1/hw1.exe

# Run homework 2
dune exec assignments/hw2/hw2.exe
```

### Run Project

The main project executable:

```bash
dune exec project/main.exe
```

### Interactive Testing with utop

You can use `utop` for interactive development and testing:

```bash
# Start utop with access to your project modules
dune utop

# Alternative method
dune exec utop
```

In the utop session, you can then load and test your utility functions:

```ocaml
(* Load your utility modules *)
open Evolutionary_computing.Util;;

(* Test functions interactively *)
(* Example assuming you have functions in your util library *)
```

## Troubleshooting

### Common Issues

**Build errors after switching OCaml versions:**
```bash
dune clean
dune build
```

**Environment not set up:**
```bash
eval $(opam env)
```

**Missing packages:**
```bash
opam install <package-name>
```

**Check OCaml and Dune versions:**
```bash
ocaml --version
dune --version
```

## Notes

* The utility functions in `util/` are automatically available to all assignment and project executables through the dune build system
* No additional setup is required beyond the initial installation - `dune build` handles compilation and linking automatically
* Ensure your OCaml and Dune versions match the requirements to avoid compatibility issues
* If you encounter permission issues during installation, you may need to use `sudo` with the system package manager commands, but never with `opam` commands

## Getting Help

If you encounter any issues with the setup or have questions, please reach out at [corniedj@mail.uc.edu](mailto:corniedj@mail.uc.edu).

## Additional Resources

* [OCaml Documentation](https://ocaml.org/docs)
* [Dune Documentation](https://dune.readthedocs.io/)
* [OPAM Documentation](https://opam.ocaml.org/doc/)
