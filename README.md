## Building
1. `git clone --depth 1 -b ghc-8.10.2-release https://gitlab.haskell.org/ghc/ghc ghc-8.10.2/`
2. `cd ghc-8.10.2/`
3. `git submodule update --init --recursive`
4. `git submodule sync`
5. `cd ..`
6. `docker run --interactive --tty --rm -v $(pwd)/ghc-8.10.2:/home/packager/ghc --workdir=/home/packager/ghc/ --name lazy-analysis-build-env haskell-lazy-analysis:v0.2.6`
7. (inside the container) `./boot && ./configure`
8. (inside the container) `./hadrian/build.sh -j --flavour=devel2`

