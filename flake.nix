# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc944;

        cabal-install-wrapper = pkgs.writeScriptBin "cabal"
          ''
          #!${pkgs.bash}/bin/bash
          ${pkgs.haskellPackages.cabal-fmt}/bin/cabal-fmt -i *.cabal
          ${pkgs.cabal-install}/bin/cabal $@
          '';

        hls-wrapper = pkgs.writeScriptBin "haskell-language-server"
          ''
          #!${pkgs.bash}/bin/bash
          PATH=${pkgs.cabal-install}/bin:$PATH ${haskellPackages.haskell-language-server}/bin/haskell-language-server $@
          '';

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "types";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            hls-wrapper
            # haskellPackages.haskell-language-server
            ghcid
            cabal-install-wrapper
            # cabal-install
            haskellPackages.cabal-fmt
            haskellPackages.alex
            haskellPackages.happy
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
