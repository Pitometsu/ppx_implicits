#!/bin/sh
set -e
rm -f $1 # make sure we do not test old execs
ocamlc -ppx ../ppx/ppx_implicits -o $1 $1.ml
./$1

