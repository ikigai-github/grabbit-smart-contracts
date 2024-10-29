FROM nixos/nix
WORKDIR /src

COPY ./docker/nix.conf /etc/nix/nix.conf

RUN nix-channel --update

RUN nix-env -iA nixpkgs.haskellPackages.cabal-install

COPY . .

RUN nix develop .\#onchain --command cabal run vulcan-scripts-compile
