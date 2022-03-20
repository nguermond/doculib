## Build from source (linux):
1. Install [opam](https://opam.ocaml.org/):
```
sudo apt install opam
```
2. Install OCaml >= 4.12.0 (if not already installed)
```
opam switch create 4.12.0 && eval $(opam env)
```
3. Install system dependencies:
```
sudo apt install libgmp-dev pkg-config libcairo2-dev libexpat1-dev libgtk-3-dev
```
4. Get source:
```
git clone https://github.com/nguermond/doculib.git
```
5. Install:
```
cd doculib && opam install .
```

## Build from source (MacOS):
1. Install [opam](https://opam.ocaml.org/):
```
brew install opam
```
2. Install OCaml >= 4.12.0 (if not already installed):
```
opam switch create 4.12.0 && eval $(opam env)
```
3. Get source:
```
git clone https://github.com/nguermond/doculib.git
```
4. Install:
```
cd doculib && opam install .
```
