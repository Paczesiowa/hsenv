#!/bin/sh

# TODO: exit if VIRTHUALENV is defined

ENV=$1
VIRTHUALENV="$(pwd)/$ENV"
GHC_ARCH_OS_VER=i386-linux-7.0.4
GHC_PACKAGE_PATH="$VIRTHUALENV/.ghc/$GHC_ARCH_OS_VER/package.conf.d"

mkdir $1
cd $1
mkdir .cabal
ghc-pkg init $GHC_PACKAGE_PATH

BOOT_PACKAGES="ffi rts ghc-prim integer-gmp base array containers filepath old-locale old-time unix directory pretty process Cabal bytestring ghc-binary bin-package-db hpc template-haskell ghc"

for package in $BOOT_PACKAGES; do
    ghc-pkg describe $package | (GHC_PACKAGE_PATH=$GHC_PACKAGE_PATH ghc-pkg register -)
done

cat ~/projects/virthualenv/cabal_config | sed "s:<VIRTHUALENV>:$VIRTHUALENV:g" | sed "s:<GHC_PACKAGE_PATH>:$GHC_PACKAGE_PATH:g" > $VIRTHUALENV/.cabal/config

# GHC_PACKAGE_PATH=$GHC_PACKAGE_PATH cabal --config-file=$VIRTHUALENV/.cabal/config update
cp -r /tmp/packages $VIRTHUALENV/.cabal/

mkdir $VIRTHUALENV/.virthualenv
mkdir $VIRTHUALENV/.virthualenv/bin

cat ~/projects/virthualenv/activate | sed "s:<VIRTHUALENV>:$VIRTHUALENV:g" | sed "s:<GHC_PACKAGE_PATH>:$GHC_PACKAGE_PATH:g" >> $VIRTHUALENV/.virthualenv/bin/activate

cp ~/projects/virthualenv/cabal $VIRTHUALENV/.virthualenv/bin/
