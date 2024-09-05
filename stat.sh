#!/bin/bash

set -e

for f in $@ ; do
    echo $f
    objdump -h $f | grep -E '.rodata|.text|.data' | while IFS= read -r l; do
        L=($l)
        SUM=$((SUM + "0x${L[2]}"))
        echo ${L[2]} ${L[1]} $SUM
    done
done