#!/bin/bash
SERVERS="01 02 03 04 05"
for s in $SERVERS; do
    echo "copying to fi--didelx${s}"
    scp *.sh "fi--didelx${s}:rbuild"
done
