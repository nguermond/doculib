export PATH := /usr/local/bin:$(PATH)

build:
	opam init --compiler=4.12.0 --yes 
	eval $(opam env)
	opam install opam-depext
	opam install . --deps-only
	opam exec -- dune build

install:
	mv _build/default/doculib.exe ${HOME}/.local/bin/doculib
