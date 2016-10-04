#!/bin/bash
SERVERS="01 02 03 04 05"
for s in $SERVERS; do
    echo -n "fi--didelx${s}:"
    ssh "fi--didelx${s}" "ls /opt/local/R | paste -d, -s"
done
