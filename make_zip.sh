#!/bin/bash

DIR=piotr_majcherczyk

mkdir $DIR
cp Makefile_nocabal $DIR/Makefile
cp -R bad good EBNF $DIR
cp README* Main.hs CXBase.hs CXTypeChecking.hs $DIR

zip -r $DIR $DIR
rm -rf $DIR
