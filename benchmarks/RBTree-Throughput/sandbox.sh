#!/bin/bash

set -e

if [ "$#" -ne 2 ]; then
  echo "usage: $0 ghc-flavor sandbox-name"
  exit
fi

sb=.cabal-sandbox-$2
d=`pwd`
ghcd=/localdisk/ryates/ghc-7.10/ghc-$1-build

if [ ! -x "$ghcd/bin/ghc" ]; then
  echo "Could not find ghc: $ghcd/bin/ghc"
  exit
fi

ghc=`$ghcd/bin/ghc --version | ghc -e "getContents >>= (putStrLn . last . words)"`


sb_pkgs=$d/$sb/x86_64-linux-ghc-$ghc-packages.conf.d
ghc_pkgs=$ghcd/lib/ghc-$ghc/package.conf.d

export CABAL_SANDBOX_CONFIG=$d/cabal.sandbox.config
export CABAL_SANDBOX_PACKAGE_PATH=$sb_pkgs:$ghc_pkgs
export GHC_PACKAGE_PATH=$CABAL_SANDBOX_PACKAGE_PATH

export PATH=$ghcd/bin:$PATH

echo "In sandbox"

/bin/tcsh
