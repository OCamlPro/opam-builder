ROOTDIR=`pwd`

# Version of OCaml used to bootstrap
OCAML=4.04
OCAMLMINOR=0
OCAMLPATH=${ROOTDIR}/ocaml-${OCAML}.0/local/bin:$PATH

# Location of OPAM branch to use for opam-builder
OPAM_BRANCH=2017-04-13-opam-builder
OPAM_DEV=lefessan


BINDIR=${ROOTDIR}/bin
OPAMPATH=${BINDIR}:${PATH}
LOCALROOT=${ROOTDIR}/opamroot
OPAM=${BINDIR}/opam.dev
OPAMBUILDER=${BINDIR}/opam-builder
