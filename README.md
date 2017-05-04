# opam-builder

Monitor and build the opam-repository, and display results on the web.
It uses a patched version of OPAM to avoid the recompilation of
already compiled packages (binary packages), and is completely
incremental, i.e. not recomputing anything that has not changed since
the last commit.

Currently, the results of running opam-builder on a 4-core server
running Debian 8 are displayed here:

http://opam.ocamlpro.com/builder/html/report-last.html

# Build

## Dependencies

The following dependencies are needed:
 * ocp-build >= 1.99.19-beta
 * OCaml >= 4.04
 * aspcud (at least the version of Debian 8)
 * jsonm

## Compilation

Then, run the following:

```
./configure
make
```

## Patched version of OPAM

You also need a patched version of `opam`. Use the branch:

https://github.com/lefessan/opam/tree/2017-04-13-opam-builder

You should compile this patched version of `opam`, and install the
resulting binary in your PATH under the name `opam.dev`.

## Depexts

Since you are going to build the entire opam-repository, you should
make sure you have all the external dependencies available. This is
usually done by calling `opam depexts`. You might want to do it using
your standard OPAM setting before installing opam-builder.

# Usage

See some scripts in the `scripts` directory:

* `install-builder.sh`: this script will setup a directory BUILDER
    with two switches and a monitoring daemon. You should probably not
    run that script, but use it as a list of commands to use to setup
    your environment.
    
* `restart-switch.sh`: this script can be used in a switch, to launch
    a daemon watching this switch.
    
* `restart-www.sh`: this script can be used (after modification) to
    launch a daemon that will generate the JSON files when new reports
    are published.
    
