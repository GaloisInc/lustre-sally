# Overview

The `lustre-sally` tool can verify systems specified using the Lustre
language by converting Lustre specifications into a state-machine
representation suitable for analysis using Sally. It then invokes the
Sally model checker on this state-machine model.

# Building `lustre-sally`

Lustre-Sally is written in Haskell and can be built with the Stack build
system. If you have a recent version of Stack installed, it should be
sufficient to clone the `lustre-sally` repository and run `stack build`
within it.

    git clone https://github.com/GaloisInc/lustre-sally
    cd lustre-sally
    git submodule update --init
    stack build

Optionally, the resulting binaries can be installed into the standard
Stack binary directory as follows:

    stack install

On most Unix-like system, this command will copy the binaries into
`~/.local/bin`. If this directory is in your `PATH`, then you can invoke
the tool `lustre-sally` from a shell. It is also possible to run the
tool from the `lustre-sally` directory as follows:

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

## Example

Here is an example Lustre model (file `tests/jkind/integrate.lus` in the
distribution). More examples are included in directory `tests/jkind`.

~~~~
node integ(x : int) returns (sum : int);
let
  sum = x + (0 -> pre sum);
tel;

node main(x : int; y : int) returns (z : int);
var
  history : bool;
  prop1 : bool;
  prop2 : bool;
let
  z = integ(x);

  history = x > 0 and (true -> pre history);
  prop1 = history => z > 0;

  --%PROPERTY prop1;

  prop2 = integ(x) + integ(y) = integ(x + y);
  --%PROPERTY prop2;
tel;
~~~~


This file has two properties defined in node main. This node takes two
input `x` and `y` and produces an output `z`. In Lustre, these input and
output variables are called flows and denote infinite sequences of
values (in this case, integer values). The model also includes Boolean
flows history, `prop1` and `prop2` that specify properties we want to
verify. The Lustre model can be interpreted as a set of equations that
output flows in terms of the input flows.

For example, node integ takes an input flow `x` and produces an output
flow `sum` defined by the equation

    sum = x + (0 -> pre sum).

One can interpret `x` and `sum` as sequences of integers ($x_{t}$) and
($\mathit{sum}_{t}$) for $t \in \mathbb{N}$. Then the previous equation means

$$
\mathit{sum}_0 = 0
$$
$$
\mathit{sum}_{t+1} = \mathit{sum}_{t} + x_{t}
$$

In the rest of the model, `z` is the sum of the past values of `x` and
history is true as long as all past values of `x` are positive. The flow
`prop1` should then be always true: if all past values of `x` are
positive, then their sum is positive. Similarly, `prop2` should be
always true.

We can verify the two property by invoking lustre-sally as follows:

    stack run lustre-sally tests/jkind/integrate.lus

The tool will confirm that both `prop1` and `prop2` are valid (that is
they are always true).

## Command-Line Options

Several command-line options are available:

* `-n IDENT`, `--node=IDENT`
    * Translate this node (instead of all nodes)

* `-d DIR`, `--out-dir=DIR`
    * Save output in this directory (default: `results`)

* `--save-core`
    * Save Core Lustre output. This is a simplified version of the
      original file, but still in Lustre format.

* `--save-sally`
    * Save Sally output. Useful mostly for debugging.

* `--yices-mode=hybrid|dpllt|mcsat`
    * Specify how Sally should invoke the Yices SMT solver.

* `--counter-example-limit=N`
    * Set the size of the largest counter-example to look for.

* `--counter-example-lower-limit=N`
    * Set the size of the smallest counter-example to look for.

* `--proof-depth=N`
    * Set the number of previous states to consider during inductive
      proof.

* `--timeout=N`
    * Terminate `sally` invocations after `N` seconds.

* `-c FILE`, `--config=FILE`
    * Read configuration from `FILE`. The format of this file is
      described in the "Running a Docker image" section.

* `--test-mode`
    * Run in testing mode, which prints more intermediate results.

* `--no-trace`
    * Don't produce trace files when proofs fail.

* `--version`
    * Print out the version of `lustre-sally`.

* `--xml`
    * Produce output in an XML format intended to mimic the output of
      Kind 2 with the `-xml` flag.

* `--help`
    * See all avaialable command line options.

# Docker integration

The Dockerfile in this repository can be used to build an image
containing an up to date version of `lustre-sally`, `sally`, and
`yices`.

## Building a Docker image

The following steps can be used to build an image:

    1. ./build-utils/prep-docker-build
    2. docker build --tag lustre-sally .

## Running a Docker image

The Docker image needs two directories on the host: one for inputs
and one for outputs.  See `build-utils/run-docker` for an example of how to
invoke `docker`.

The input directory should contain Lustre models with
queries---`lustre-sally` will process each Lustre model, and store the
results in the ouput directory.

The input directory may also contain a file call `settings`, which can
provide additional configuration for `lustre-sally`.  The format of this
file is as a list of `key: value` pairs, where the `key`s are the same
as the long names of the command line options for `lustre-sally` and
the values are the corresponding command line flags.  Here is an example
of a simple `settings` file:

    counter-example-limit: 25
    counter-example-lower-limit: 25
    proof-depth: 1
