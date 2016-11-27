#!/bin/sh

echo Are you sure OCaml is not in the PATH ?
sleep 5

TEMPLATE=4.02.1
SWITCH=$1

mkdir ${SWITCH}
cp -r ${TEMPLATE}/.git ${SWITCH}/.git
cp ${TEMPLATE}/restart.sh ${SWITCH}/
cd ${SWITCH}
git checkout .
./restart.sh
echo Now:
echo 1/ Please edit .opam/config to set jobs to 1
echo 2/ Edit www/restart.sh to add ${SWITCH}, kill it and restart it
echo 
sleep 2
tail -f nohup.out
