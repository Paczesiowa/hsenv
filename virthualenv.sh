#!/bin/bash

if [ -n "${VIRTHUALENV}" ]; then
    echo "There is already active ${VIRTHUALENV_NAME} Virtual Haskell Environment (at ${VIRTHUALENV})."
    echo "Deactivate it first (using 'deactivate' command), to create another environment."
    exit 1
fi

VIRTHUALENV_SCRIPT_DIR="$(dirname ${0})"
CABAL_CONFIG_SKEL="${VIRTHUALENV_SCRIPT_DIR}/cabal_config"
CABAL_WRAPPER_SKEL="${VIRTHUALENV_SCRIPT_DIR}/cabal"
ACTIVATE_SKEL="${VIRTHUALENV_SCRIPT_DIR}/activate"

ENV=$1
VIRTHUALENV="$(pwd)/$ENV"
VIRTHUALENV_NAME="${ENV}"
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

cat "${CABAL_CONFIG_SKEL}" | sed "s:<VIRTHUALENV>:$VIRTHUALENV:g" | sed "s:<GHC_PACKAGE_PATH>:$GHC_PACKAGE_PATH:g" > $VIRTHUALENV/.cabal/config

# GHC_PACKAGE_PATH=$GHC_PACKAGE_PATH cabal --config-file=$VIRTHUALENV/.cabal/config update
cp -r /tmp/packages $VIRTHUALENV/.cabal/

mkdir $VIRTHUALENV/.virthualenv
mkdir $VIRTHUALENV/.virthualenv/bin

cat "${ACTIVATE_SKEL}" | sed "s:<VIRTHUALENV_NAME>:$VIRTHUALENV_NAME:g" | sed "s:<VIRTHUALENV>:$VIRTHUALENV:g" | sed "s:<GHC_PACKAGE_PATH>:$GHC_PACKAGE_PATH:g" >> $VIRTHUALENV/.virthualenv/bin/activate

CABAL_BINARY="$(which cabal)"
cat "${CABAL_WRAPPER_SKEL}" | sed "s:<CABAL_BINARY>:${CABAL_BINARY}:g" > "${VIRTHUALENV}/.virthualenv/bin/cabal"
chmod +x "${VIRTHUALENV}/.virthualenv/bin/cabal"
