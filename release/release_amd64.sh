#!/bin/sh
set -x -e
cd "$(dirname "$0")"
DIR=doculib
BIN=$DIR/usr/local/bin

mkdir -p $BIN
rm -f $BIN/doculib
cp ../_build/default/doculib.exe $BIN/doculib
dpkg-deb --build --root-owner-group $DIR packages
