#!/bin/bash


cd src/
FILES="Main.hs" 
echo $FILES
haddock -h -o ../haddock Main.hs
