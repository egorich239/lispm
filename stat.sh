#!/bin/bash

set -e

readonly FILES=(lispm.o lrt0.o lispm-funs.o)

for f in ${FILES[@]} ; do
    echo $f
    objdump -h $f | grep -E '.rodata|.text|.data' | while IFS= read -r l; do
        L=($l)
        SUM=$((SUM + "0x${L[2]}"))
        echo ${L[2]} ${L[1]} $SUM
    done
done