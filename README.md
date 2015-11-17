# GenSolver

## Build Status

Mono | .NET
---- | ----
[![Mono CI Build Status](https://img.shields.io/travis/halcwb/Bootstrap/master.svg)](https://travis-ci.org/halcwb/Bootstrap) | [![.NET Build Status](https://img.shields.io/appveyor/ci/halcwb/Bootstrap/master.svg)](https://ci.appveyor.com/project/halcwb/Bootstrap)


## Background



## Setup



## Building
The build can be started by running:

* `build.sh` on unix systems (tested on max os x)
* `build.cmd` on windows systems

Running `build.cmd` or `./build.sh` is the same as running the command with the `ALL` argument. To list all build targets use the `--listTargets` or `-lt` argument.

## Bootstrapping
The bootstrapping mechanism does the following:

1. Check if a paket.exe exists
2. Download the paket.exe if not
3. Use the paket.exe to install dependencies


## Testing
Testing will be run after the build. As specified by the testing target in the `build.fsx` script.

## Continuous integration
This setup assumes that the current branch is a development branch. There is a specific integration target that can be run by `./build.sh integrate` or `build.cmd integrate`:

1. First it checks whether all changes are commited, i.e. the current repos is clean.
2. Then the latest Appveyor (.net) and Travis (mono) builds are checked whether they passed.
3. Then the master branch is checked out and updated with the `remotes/origin/master` branch.
3. The current branch is merged with the master branch and pushed to GitHub.
4. Finally the current branch is checkout out to resume development.


## Documentation


## Publishing
TODO


## Deployment
TODO

