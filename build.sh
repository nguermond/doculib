#!/bin/sh

dune build
mkdir $HOME/.doculib
mkdir $HOME/.doculib/icons
cp icons/Gnome-colors-applications-office.svg $HOME/.doculib/icons/Gnome-colors-applications-office.svg
mv _build/default/doculib.exe $HOME/.local/bin/doculib
