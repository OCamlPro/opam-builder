#!/bin/sh

# The goal of this script is to run the following architecture:
# * One daemon watching the opam-repository to a local copy
# * One daemon watching the reports from switches, and generating the JSON file
# * One daemon per switch, to watch the local opam-repo and build the file,
#    and generate a report per commit

# You need 'wget' installed to run this script
# For now, it will only create switches 4.02.1 and 4.03.0. 
# You should use it only as an example to set up your configuration.

mkdir BUILDER
cd BUILDER

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

mkdir ${BINDIR}

# Download and extract OCaml
cd ${ROOTDIR}
wget http://caml.inria.fr/pub/distrib/ocaml-${OCAML}/ocaml-${OCAML}.${OCAMLMINOR}.tar.gz
tar zxf ocaml-${OCAML}.${OCAMLMINOR}.tar.gz

# Build OCaml and install it locally
cd ${ROOTDIR}/ocaml-${OCAML}.${OCAMLMINOR}
./configure --prefix ${ROOTDIR}/ocaml-${OCAML}.${OCAMLMINOR}/local
make world.opt
make install

# Download and extract OPAM
cd ${ROOTDIR}
git clone https://github.com/ocaml/opam.git
cd ${ROOTDIR}/opam
git remote add ${OPAM_DEV} https://github.com/${OPAM_DEV}/opam
git fetch ${OPAM_DEV}
git checkout -b ${OPAM_BRANCH} --track ${OPAM_DEV}/${OPAM_BRANCH}
cd ..

# Build OPAM
cd ${ROOTDIR}/opam
PATH=${OCAMLPATH} ./configure
PATH=${OCAMLPATH} make lib-ext
PATH=${OCAMLPATH} make

# opam-builder expects opam to be called opam.dev
cp -f src/opam ${OPAM}

# Download and extract opam-repository
cd ${ROOTDIR}
git clone https://github.com/ocaml/opam-repository
cd ${ROOTDIR}/opam-repository
git remote add ocaml https://github.com/ocaml/opam-repository
git fetch ocaml

# Create a local OPAM switch for opam-builder, with its dependencies
OPAMROOT=${LOCALROOT} ${OPAM} init --comp=4.05.0 -q
OPAMROOT=${LOCALROOT} ${OPAM} install dose3
OPAMROOT=${LOCALROOT} ${OPAM} install ocp-build
OPAMROOT=${LOCALROOT} ${OPAM} install jsonm

# Download and extract opam-builder
cd ${ROOTDIR}
git clone https://github.com/OCamlPro/opam-builder

# Build opam-builder
cd ${ROOTDIR}/opam-builder
OPAMROOT=${LOCALROOT} ${OPAM} exec ./configure
OPAMROOT=${LOCALROOT} ${OPAM} exec make
cp _obuild/opam-builder/opam-builder.asm ${OPAMBUILDER}


cd ${ROOTDIR}
# all instances share a download-cache
mkdir opam-download-cache

# Now, here is how to create a switch for 4.02.1:

PATH=${OPAMPATH} ${OPAMBUILDER} create 4.02.1
cd ${ROOTDIR}/4.02.1

# update the switch index
cd 2.0
PATH=${OPAMPATH} ${OPAM} admin index
cd ..

# watch the 4.02.1 switch
PATH=${OPAMPATH} nohup ${OPAMBUILDER} switch &

# Now, here is how to create a switch for 4.03.0:

# You can use `scripts/restart-switch.sh` for that
PATH=${OPAMPATH} ${OPAMBUILDER} create 4.03.0
cd ${ROOTDIR}/4.03.0
cd 2.0
PATH=${OPAMPATH} ${OPAM} admin index
cd ..
PATH=${OPAMPATH} nohup ${OPAMBUILDER} switch &

# The JSON watcher
cd ${ROOTDIR}
mkdir ${ROOTDIR}/www

# You can use `scripts/restart-www.sh` for that
cd ${ROOTDIR}/www
${OPAMBUILDER} json --watch ..

# We must also monitor the remote opam-repo to update the local repos
cd ${ROOTDIR}/opam-repository
nohup ${OPAMBUILDER} opam &


