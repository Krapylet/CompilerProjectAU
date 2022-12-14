#!/bin/sh
cp rescue/dunefiles/src.compiler.dune.rescue src/compiler/dune
cp rescue/dunefiles/src.compiler.main.dune.rescue src/compiler/main/dune
sed "s/native/`uname`/g" rescue/dunefiles/rescue.native.sexp > src/compiler/main/rescue.native.sexp
sed "s/byte/`uname`/g" rescue/dunefiles/rescue.byte.sexp > src/compiler/main/rescue.byte.sexp
rm -f CONFIGURED-NORM
touch CONFIGURED-RESCUE
