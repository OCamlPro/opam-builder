#!/bin/sh

# This script is supposed to be run in a directory that your webserver
# can access. For this reason, ROOTDIR should be specified as an
# absolute directory.

ROOTDIR=/path-to-opam-builder/BUILDER

nohup ${ROOTDIR}/bin/opam-builder json --watch ${ROOTDIR} &

tail -f nohup.out
