#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling chiton
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/cloudsoft/Code/advent-of-code/2021/lib/x86_64-darwin/chiton.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/cloudsoft/Code/advent-of-code/2021/lib/x86_64-darwin/chiton.s
if [ $? != 0 ]; then DoExitAsm chiton; fi
rm /Users/cloudsoft/Code/advent-of-code/2021/lib/x86_64-darwin/chiton.s
echo Assembling adventofcode
/Library/Developer/CommandLineTools/usr/bin/clang -c -o /Users/cloudsoft/Code/advent-of-code/2021/lib/x86_64-darwin/adventOfCode.o  -arch x86_64 -mmacosx-version-min=10.8 -x assembler /Users/cloudsoft/Code/advent-of-code/2021/lib/x86_64-darwin/adventOfCode.s
if [ $? != 0 ]; then DoExitAsm adventofcode; fi
rm /Users/cloudsoft/Code/advent-of-code/2021/lib/x86_64-darwin/adventOfCode.s
echo Linking /Users/cloudsoft/Code/advent-of-code/2021/adventOfCode
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -framework Cocoa      -multiply_defined suppress -L. -o /Users/cloudsoft/Code/advent-of-code/2021/adventOfCode `cat /Users/cloudsoft/Code/advent-of-code/2021/link.res` -filelist /Users/cloudsoft/Code/advent-of-code/2021/linkfiles.res
if [ $? != 0 ]; then DoExitLink /Users/cloudsoft/Code/advent-of-code/2021/adventOfCode; fi
IFS=$OFS
