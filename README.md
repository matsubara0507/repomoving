# repomoving

## Requirement

## Usage

## Build

### Docker

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/repomoving . --build-arg local_bin_path=./bin
```
