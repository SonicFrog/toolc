#!/bin/sh

rm "$1_"*

touch "$1_tes"
javap -c "$1.class" >> "$1_tes"

java -jar toolc-reference-2.6.jar programsGiven/Expression.tool
touch "$1_ref"
javap -c "$1.class" >> "$1_ref"

diff -y "$1_ref" "$1_tes"