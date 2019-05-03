# Building `lustre-sally`

## Using Stack

If you have a recent version of Stack installed, it should be
sufficient to clone this repository and run `stack build` within it.

To run the resulting binary, either use `stack install` to copy it
into the standard Stack binary directory (usually `~/.local/bin` on
most Unix-like systems) or use

    stack run lustre-sally -- <further options>

If you're running on a single input file, with no command-line
options, the `--` can be omitted.

# Using `lustre-sally`

The `lustre-sally` program checks Lustre models to determine whether
properties embedded within the models are always true. It uses the
Sally model checker to do the core analysis work.

It takes as input a single Lustre file that may have embedded
property descriptions.

For each property, it prints out whether the property is always true
("Valid"), sometimes false ("Invalid"), or could not be determined
("Unknown").

In the case where a property is invalid, `lustre-sally` will generate
an interactive HTML trace of the sequence of states leading to the
property violation.

Several command-line options are available:

* -n IDENT, --node=IDENT
    * Translate this node (instead of all nodes)

* -d DIR, --out-dir=DIR
    * Save output in this directory (default: `results`)

* --save-core
    * Save Core lustre output in this file. This is a simplified
      version of the original file, but still in Lustre format.

* --save-sally
    * Save Sally output in this file. Useful mostly for debugging.

* --help
    * See all avaialable command line options.


# Docker integration

The provided Dockerfile can be used to build an image containing an up
to date version of `lustre-sally` and `sally`.

## Building a docker image

The following steps can be used to build an image:

    1. ./build-utils/prep-docker-build
    2. docker build --tag lustre-sally .

## Running a docker image

The docker image needs two directories on the host: one for inputs
and one for outputs.  See `build-utils/run-docker` for an example of how to
invoke `docker`.

The input directory should contain Lustre models with queries---`lustre-sally`
will process each Lustre model, and store the results in the ouput directoyr.

The input directory may also contain a file call `settings`, which can
provide additional configuration for `lustre-sally`.  The format of this
file is as a lit of `key: value` pairs, where the `key`s are the same
as the long names of the command line options for `lustre-sally` and
the values are the correcponding command line flags.  Here is an example
of a simple `settings` file:

    counter-example-limit: 25
    counter-example-lower-limit: 25
    proof-depth: 1










