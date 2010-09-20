#!/usr/bin/env bash

# Putting stuff in variables and expanding inside double quotes seems to be
# safe.

v0=$'$PWD'
v1=$'$PWD`date`x'
v2=$'$PWD\n$MU\n$SHELL'
v3=$'$PWD\b"$PWD"\n$SHELL'

echo "----"
echo $v0
echo "$v0"
echo "----"
echo $v1
echo "$v1"
echo "----"
echo $v2
echo "$v2"
echo "----"
echo $v3
echo "$v3"


