#!/bin/sh

dune build
mv _build/default/doculib.exe /usr/local/bin/doculib
