## Setup

### Building the Docker image
1. clone this repository and `cd` into it
2. `docker build --compress -t haskell-laziness-analysis:v0.2.6 .`

### Building GHC
1. `git clone --depth 1 -b ghc-8.10.2-release https://gitlab.haskell.org/ghc/ghc ghc-8.10.2/`
2. `cd ghc-8.10.2/`
3. `git submodule update --init --recursive`
4. `git submodule sync`
5. `cd ..`
6. apply the patches of this project, mainly the Hadrian `UserSettings.hs` with the `debug_rts` build flavour
7. `docker run --interactive --tty --rm -v $(pwd)/ghc-8.10.2:/home/packager/ghc --workdir=/home/packager/ghc/ --name laziness-analysis-build-env haskell-laziness-analysis:v0.2.6`
8. (inside the container) `./boot && ./configure`
9. (inside the container) `hadrian/build.sh -j --flavour=debug_rts 'stage1.ghc-bin.ghc.link.opts += -debug'`


## Development
You can fetch the entire Git history of GHC using the commands listed [here](https://stackoverflow.com/a/17937889/10384819), which is useful for enabling `git log`, `git blame`, etc. This will also fetch newer commits and branches, seemingly even for submodules. Depending on your connection, `git fetch --unshallow` can take 5-10 minutes.

### Debugging GHCi
1. (outside the container) `ghc-8.10.2/_build/stage1/bin/ghc --interactive -ddump-bcos +RTS -Di`

