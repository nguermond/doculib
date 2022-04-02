build:
	opam init --compiler=4.12.0 --yes 
	eval $(opam env)
	opam install opam-depext
	opam install . --deps-only
	opam exec -- dune build

install:
	mkdir -p ${HOME}/.doculib
	mv _build/default/doculib.exe ${HOME}/.local/bin/doculib
