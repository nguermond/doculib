install:
	opam init --compiler=4.12.0 --yes 
	eval $(opam env)
	opam install . --yes
