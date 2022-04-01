all:
	opam init --compiler=4.12.0 --yes 
	eval $(opam env)
	opam install . --yes

install:
	mkdir -p ${HOME}/.doculib
	mkdir -p ${HOME}/.doculib/icons
	cp icons/Gnome-colors-applications-office.svg ${HOME}/.doculib/icons/Gnome-colors-applications-office.svg
	mv _build/default/doculib.exe ${HOME}/.local/bin/doculib
