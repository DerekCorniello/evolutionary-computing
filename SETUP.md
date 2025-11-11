# Evolutionary Computing - Setup Instructions

This repository contains assignments, a project, and a reusable utility library for the Evolutionary Computing course.

## For Professors/Graders

This document provides setup instructions for students, but includes guidance for grading. The course focuses on evolutionary algorithms (e.g., genetic algorithms for optimization), implemented in OCaml. Assignments build on each other, with tasks in each assignment folder (e.g., task1.ml for basic functions, task2.ml for max-ones problem, task3.ml for Rosenbrock function). Outputs are saved as .txt files for easy review, similar to C program logs. OCaml modules are like C header files, providing reusable functions.

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
│  └─ assignmentX/
│     ├─ dune
│     └─ taskX.ml
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

* OCaml (version unspecified, but 5.2.0 recommended)
* Dune 3.x or later
* OPAM (OCaml package manager, similar to apt or package managers in C environments)
* gnu-plot (for plotting outputs)
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
opam install dune utop gnu-plot
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
# Ensure dependencies are all installed
opam install . --deps-only

# Build all executables and libraries
dune build

# Clean build artifacts (if needed)
dune clean
```

This compiles the utility library, assignments, and project executables.

### Viewing and Grading Assignments

To run and inspect assignments:

```bash
# Build first
dune build

# Run a specific task (e.g., assignment 1, task 1)
dune exec assignment1-task1

# View output files (results saved like C printf logs)
cat assignments/assignment1/corniedj-max-ones-1000-32-0_01-0_5.txt
```

Outputs include fitness values, generations, and parameters—review for correctness akin to debugging C code.

### Verify Installation

To ensure everything is working correctly, run the test installation:

```bash
dune exec test_install/test_install.exe
```

## Running Code

### Run Assignments

Each assignment executable can be ran via:

```bash
dune exec assignment1-task1
```

It is always useful to check the code first to ensure you run what you re looking for!

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

**Build errors after switching OCaml versions (similar to gcc compilation issues):**
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

If dune build fails, check for missing dependencies like gcc errors in C—contact the student for setup help.

## Notes

* The utility functions in `util/` are automatically available to all assignment and project executables through the dune build system
* No additional setup is required beyond the initial installation - `dune build` handles compilation and linking automatically
* Ensure your OCaml and Dune versions match the requirements to avoid compatibility issues
* If you encounter permission issues during installation, you may need to use `sudo` with the system package manager commands, but never with `opam` commands

## Getting Help

If you encounter any issues with the setup or have questions, please reach out at [corniedj@mail.uc.edu](mailto:corniedj@mail.uc.edu).

## Additional Resources

* [OCaml Documentation](https://ocaml.org/docs)
* [Dune Documentation](https://dune.readthedocs.io/) (like make for C projects)
* [OPAM Documentation](https://opam.ocaml.org/doc/)
* [Transitioning from C to OCaml](https://ocaml.org/docs/transition-from-c) (for C programmers)
