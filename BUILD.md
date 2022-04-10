## Build from source (linux):
1. Install [opam](https://opam.ocaml.org/):
```
sudo apt install opam
```
2. Install system dependencies:
```
sudo apt install pkg-config libev-dev libgmp-dev libcairo2-dev libexpat1-dev libgtk-3-dev
```
4. Get source:
```
git clone https://github.com/nguermond/doculib.git
```
3. Build:
```
cd doculib && make
```
5. Install:
```
make install
```

## Build from source (MacOS):
1. Install [opam](https://opam.ocaml.org/):
```
brew install opam
```
2. Install system dependencies:
```
brew install pkg-config libev gmp cairo expat gtk+3
```
3. Get source:
```
git clone https://github.com/nguermond/doculib.git
```
4. Build:
```
cd doculib && make
```
5. Install:
```
make install
```
