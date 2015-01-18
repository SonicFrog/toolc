#!/bin/sh

touch "$1_tes"
javap -c "$1.class" >> "$1_tes"
diff -y "$1_ref" "$1_tes"