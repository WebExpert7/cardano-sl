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
	binary-cache-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=

## Run the nix build

    nix-build -A cardano-sl-wallet


see `buildlog.txt`

## create : topology-mainnet
```
wallet:
  relays: [[{ host: relays.cardano-mainnet.iohk.io }]]
  valency: 1
  fallbacks: 7
```

## confirm your version

Check that your have the right binary selected
`git log -1` give you your current version `commit 2395acaba1ff0659a377a0799fb6330142a870fd`
and  `./result/bin/cardano-node --version` matches that `cardano-node-1.0.3, git revision 2395acaba1ff0659a377a0799fb6330142a870fd`

And check the md5sum for later
`md5sum ./result/bin/cardano-node` gives you `419ce13bc9d6864df25226112289dcd8  ./result/bin/cardano-node`

## Run the node
The directory `state-wallet-mainnet` can be anything and is created on startup.
```
./result/bin/cardano-node   \
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


see `connect.log`

And please check your running version

### Check version

    ps xauf | grep cardano-node
    

see the system running

    ./result/bin/cardano-node --web --no-ntp --configuration-file .

check the port : `lsof -i :8090`

```
COMMAND     PID    USER   FD   TYPE  DEVICE SIZE/OFF NODE NAME
cardano-n 32073 mdupont   42u  IPv4 8117275      0t0  TCP localhost:8090 (LISTEN)
cardano-n 32073 mdupont   44u  IPv4 8136402      0t0  TCP localhost:8090->localhost:38816 (ESTABLISHED)
```

## go on to the front end build.
see https://github.com/h4ck3rm1k3/daedalus

Note I am running a two daedalus versions

### Midtier
For the midtier I am running via 

    NODE_ENV=development node --preserve-symlinks -r babel-register webpack/server.js 

commit ab6aab0c5203e9bad9af66d17ad65b460f61f226 on master in git@github.com:input-output-hk/daedalus.git

### Port forwarding 

I connect the front and back end via ssh

    ssh -L 4000:localhost:4000 -L 8090:localhost:8090 debian-build-speed

### And the front end
This is running on my hot hack branch were I remove uneeded code so that it loads faster

`bash -x ./run.s`

```
+ HOT=0
+ NODE_ENV=development
+ ./node_modules/.bin/electron -r babel-register -r babel-polyfill ./electron/main.development
```

branch hothack on commit 40e65edaf0d88729d00e2bcadbb54c081ab35aa5 in URL: git@github.com:h4ck3rm1k3/daedalus.git
	

# Older docs

For the normal docs see the main branch
https://github.com/input-output-hk/cardano-sl

## License

Cardano SL is released under the terms of the [MIT license](https://opensource.org/licenses/MIT). Please see [LICENSE](https://github.com/input-output-hk/cardano-sl/blob/master/LICENSE) for more information.
<!-- CARDANO_SL_README_END_5 -->
