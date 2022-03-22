#!/bin/sh
set -x -e
cd "$(dirname "$0")"
DIR=doculib
BIN=$DIR/usr/local/bin
LIB=$DIR/usr/local/lib

mkdir -p $BIN
mkdir -p $LIB/doculib/icons
rm -f $BIN/doculib
cp ../_build/default/doculib.exe $BIN/doculib
cp ../icons/Gnome-colors-applications-office.svg $LIB/doculib/icons
dpkg-deb --build --root-owner-group $DIR packages
