## Building
1. `git clone --depth 1 -b ghc-8.8.4-release https://gitlab.haskell.org/ghc/ghc`
2. `cd ghc/`
3. `git submodule update --init --recursive`
4. `git submodule sync`
5. `cd ..`
6. `docker run --interactive --tty --rm -v $(pwd)/ghc:/home/packager/ghc --workdir=/home/packager/ghc/ --name lazy-analysis-build-env haskell-lazy-analysis:v0.2.4`
8. (inside the container) `./boot && ./configure`
7. (inside the container) `cabal update`
9. (inside the container) `./hadrian/build.sh -j --flavour=devel2`

