# My Instructions

All instructions are for debian sid

First sync your clock

    sudo apt-get install ntpdate 
    sudo /usr/sbin/ntpdate-debian

Look for something like 
    18 Dec 10:31:39 ntpdate[2976]: adjust time server 199.102.46.80 offset -0.012009 sec

from docs/how-to/build-cardano-sl-and-daedalus-from-source-code.md

## Common build steps

The following steps are shared between the two methods of building Cardano: fetching source and deciding on a branch to be built.

Clone Cardano SL repository and go to the root directory:

    $ git clone https://github.com/input-output-hk/cardano-sl.git
    $ cd cardano-sl
    $ git checkout master

## Nix build mode (recommended)

First, prerequisite: install Nix (full instructions at https://nixos.org/nix/download.html):

    curl https://nixos.org/nix/install | sh

Two steps remain, then:

1.  To employ the signed IOHK binary cache:

        $ sudo mkdir -p /etc/nix
	$ sudo vi /etc/nix/nix.conf       # ..or any other editor, if you prefer

    ..and then add two following lines:

        binary-caches             = https://cache.nixos.org https://hydra.iohk.io
	binary-caches-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=

## Run the build

    scripts/build/cardano-sl.sh

This calls these important steps :

    stack build --ghc-options=" -Wwarn +RTS -A256m -n2m -RTS" --test --no-haddock-deps --bench --jobs=4 --no-run-tests --no-run-benchmarks --dependencies-only  cardano-sl
    stack build --ghc-options=" -Wwarn +RTS -A256m -n2m -RTS" --test --no-haddock-deps --bench --jobs=4 --no-run-tests --no-run-benchmarks --dependencies-only  cardano-sl-node


## create : topology-mainnet
```
wallet:
  relays: [[{ host: relays.cardano-mainnet.iohk.io }]]
  valency: 1
  fallbacks: 7
```

## Run the node
```
./.stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/bin/cardano-node   \
  --web                                                                   \
  --no-ntp                                                                \
  --configuration-file ./lib/configuration.yaml                           \
  --configuration-key mainnet_full                                        \
  --tlscert ./scripts/tls-files/server.crt                                \
  --tlskey ./scripts/tls-files/server.key                                 \
  --tlsca ./scripts/tls-files/ca.crt                                      \
  --log-config ./scripts/log-templates/log-config-qa.yaml                 \
  --topology "topology-mainnet"					          \
  --logs-prefix "state-wallet-mainnet/logs"                               \
  --db-path "state-wallet-mainnet/db"                                	  \
  --wallet-db-path 'state-wallet-mainnet/wallet-db'                       \
  --keyfile state-wallet-mainnet/secret.key
```

edit ./connect-to-mainnet
     bash -x ./connect-to-mainnet2 

## go on to the front end build.
see https://github.com/h4ck3rm1k3/daedalus


# Older docs

## Cardano SL

[![Build Status](https://travis-ci.org/input-output-hk/cardano-sl.svg)](https://travis-ci.org/input-output-hk/cardano-sl)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/input-output-hk/cardano-sl?branch=master&svg=true)](https://ci.appveyor.com/project/jagajaga/cardano-sl)
[![Release](https://img.shields.io/github/release/input-output-hk/cardano-sl.svg)](https://github.com/input-output-hk/cardano-sl/releases)

<!-- CARDANO_SL_README_BEGIN_1 -->
## What is Cardano SL?

Cardano SL (or Cardano Settlement Layer) is a cryptographic currency designed
and developed by [IOHK](https://iohk.io/team) in conjunction with the University
of Edinburgh, the University of Athens and the University of Connecticut. Cardano
SL is based on the Haskell implementation of the white paper
["Ouroboros: A Provably Secure Proof-of-Stake Blockchain Protocol"](https://iohk.io/research/papers/#9BKRHCSI)
by Aggelos Kiayias, Alexander Russell, Bernardo David and Roman Oliynykov.

You can think of Cardano SL as Bitcoin reimagined with a freedom to fix Bitcoin’s
design flaws. Please read ["What Makes Cardano SL Special?"](https://cardanodocs.com/introduction/#what-makes-cardano-sl-special)
for more info about similarities and differences between Cardano SL and Bitcoin.
<!-- CARDANO_SL_README_END_1 -->
<!-- CARDANO_SL_README_BEGIN_2 -->
## Beyond Settlement Layer

Cardano SL is called a "Layer" for a reason. It is the first component of
the Cardano Platform. Eventually, it will be expanded with a Control Layer,
serving as a trusted computation framework to evaluate a special
kind of proofs to ensure that a certain computation was carried out
correctly. In gaming and gambling, such systems are used for
verifying honesty of random number generation and game
outcomes. Accompanied with side chains, it will make possible to accomplish
such tasks as provably fair distribution of winnings in games. The
application of Control Layer lies well beyond gaming and gambling. Identity
management, credit system and more will be a part of Cardano Platform.
We are also aiming to evolve Daedalus, the Cardano SL [wallet application](https://github.com/input-output-hk/daedalus),
into a universal cryptocurrency wallet featuring automated
cryptocurrency trading and cryptocurrency-to-fiat transactions.
<!-- CARDANO_SL_README_END_2 -->
<!-- CARDANO_SL_README_BEGIN_3 -->
## Supported Platforms

Supported platforms are Windows, macOS and Linux. There are
[installers for Windows and macOS](https://daedaluswallet.io/#download),
which include a main node and [Daedalus wallet](https://github.com/input-output-hk/daedalus).

Linux installer is going to be released soon. For now, to
get Cardano SL on Linux, please refer to the [Building Cardano SL and Daedalus from
Source](https://cardanodocs.com/for-contributors/building-from-source) chapter.
<!-- CARDANO_SL_README_END_3 -->
<!-- CARDANO_SL_README_BEGIN_4 -->
## Cardano SL and Daedalus Bridge

Cardano SL consists of a collection of binaries that constitute
the backend, a PureScript API for the Electron-based wallet, and the
Electron-based wallet called “Daedalus”.

The source code for both Cardano SL and Daedalus Bridge can be obtained
from the [official repository](https://github.com/input-output-hk/cardano-sl).

For instructions on building Cardano, please see
the [building from source](https://cardanodocs.com/for-contributors/building-from-source/) section.

<!-- CARDANO_SL_README_END_4 -->
<!-- CARDANO_SL_README_BEGIN_5 -->
## For Contributors

Thank you for considering to help out with the source code! We welcome contributions from anyone,
and are grateful for even the smallest of fixes!

If you'd like to contribute to Cardano SL, please fork this repository, fix, commit and send a
pull request for the maintainers to review and merge into the main code base.

Please make sure your contributions adhere to our coding guidelines:

* Code must adhere to the [Serokell Haskell Style Guide](https://github.com/serokell/serokell-util/blob/master/serokell-style.md).
* Code must be documented with [Haddock](https://www.haskell.org/haddock/doc/html/index.html).
* Pull requests need to be based on and opened against the `master` branch.

Please note that this project uses a custom prelude [Universum](https://github.com/serokell/universum)
instead of the default one.

## License

Cardano SL is released under the terms of the [MIT license](https://opensource.org/licenses/MIT). Please see [LICENSE](https://github.com/input-output-hk/cardano-sl/blob/master/LICENSE) for more information.
<!-- CARDANO_SL_README_END_5 -->
