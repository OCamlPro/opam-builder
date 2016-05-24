#!/bin/sh

BASE=/home/lefessan/builder

nohup ${BASE}/opam-builder/_obuild/opam-builder/opam-builder.asm \
   --import \
   ${BASE}/3.12.1/reports \
   ${BASE}/4.00.1/reports \
   ${BASE}/4.01.0/reports \
   ${BASE}/4.02.1/reports \
   ${BASE}/4.02.3/reports \
   ${BASE}/4.03.0/reports \
   ${BASE}/repo-lint/reports \
   &
