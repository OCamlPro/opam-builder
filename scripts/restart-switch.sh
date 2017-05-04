#!/bin/sh

CURDIR=`pwd`
ROOTDIR=`dirname ${CURDIR}`

SWITCH=`cat builder.switch`
echo SWITCH: ${SWITCH}

DATE=`date +%Y%m%d-%H%M`

echo Saving nohop.out...
mv nohup.out ${DATE}-nohup.out || echo None found

BINDIR=${ROOTDIR}/bin
OPAMPATH=${BINDIR}:${PATH}
OPAMBUILDER=${BINDIR}/opam-builder

PATH=${OPAMPATH} nohup ${OPAMBUILDER} switch &

exec tail -f nohup.out
