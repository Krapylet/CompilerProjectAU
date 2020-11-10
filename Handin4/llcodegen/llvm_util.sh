#!/bin/sh
X86OUTDIR=_build/x86/
mkdir -p $X86OUTDIR
FILE=`basename $1`
EXT="${1##*.}"
LLEXT='ll'
TEMPFILE=$X86OUTDIR$FILE
TEMPFILEX86=$TEMPFILE.s
if [ $EXT != $LLEXT ]; then 
  echo "Test must end with .ll"
  exit 1
fi
_build/install/default/bin/llvm_util $1 -o $TEMPFILEX86
if [ $? -ne 0 ]; then 
  exit $?
fi
echo "ASM file generated at $TEMPFILEX86"
clang $2 $TEMPFILEX86 -o $TEMPFILE.out
if [ $? -ne 0 ]; then
  exit $?
fi
echo "Out file generated at $TEMPFILE.out"
