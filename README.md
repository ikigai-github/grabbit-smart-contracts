# Grabbit Smart Contracts

## Repo guidelines

Development on the onchain contracts should be done in feature branches and merged into `develop` via PRs. 

When the contracts used in Apollo are updated, the state of the repo at that point should be merged to `main`. Until then all onchain changes should remain in `develop`.

## Decentralised and Concurrent Auctions
The auction mechanism makes use of a Finite Set on-chain that records all the bidders enrolled within the auction. This means that every bidder has their own BidEscrow and therefore there is no contention when making bids. See [Grabbit Documentation](https://github.com/ikigai-github/grabbit-documentation) for more.

## Development:

All on-chain scripts are written in Plutarch and can be found in ./onchain

All off-chain contracts are written w/ Lucid-Evolution and can be found in [Apollo-Offchain](https://github.com/ikigai-github/apollo-offchain) (currently closed source)

To devlop within the on-chain script environment enter a nix shell with the make command
- `make shell`

run `make usage` to see additional available `make` commands in the root directory. 

## Running the Protocol:
### Backend:
Start the Script compiler Server
- `make shell`
- `make vulcan_server`

## Optional `direnv` extension for your shell

### Installation for non-NixOS systems

* Follow installation
https://github.com/direnv/direnv/blob/master/docs/installation.md

* Hook direnv into the shell
https://github.com/direnv/direnv/blob/master/docs/hook.md

### Installation using home-manager

* https://github.com/nix-community/nix-direnv#via-home-manager

### Enable direnv

* Make sure you are inside the project directory

```
direnv allow
```
now whenever you are inside the project folder Nix environment will load automatically

### More information
* https://github.com/direnv/direnv/tree/master
