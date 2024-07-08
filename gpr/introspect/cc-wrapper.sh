#!/bin/sh
cd $(dirname "$1")
BASENAME=$(basename "$1")
ld -r -b binary $BASENAME -z noexecstack -o ../../obj/introspect/$(echo $BASENAME | sed 's/\.xml/\.o/g')
