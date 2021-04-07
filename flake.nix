{
  description = "LiterateFStar";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    fstar-flake.url = "github:W95Psp/nix-flake-fstar";
  };
  
  outputs = { self, nixpkgs, flake-utils, fstar-flake }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux"]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          master = fstar-flake.defaultPackage.${system};
          tools = fstar-flake.lib.${system}.fstar;
          lib = pkgs.lib;
        in
          rec {
            fstar-libs = {
              literate-fst-lib = pkgs.stdenv.mkDerivation {
                name = "literate-fst-lib";
                src = pkgs.lib.cleanSourceWith {filter = path: type: pkgs.lib.hasSuffix ".fst" path; src = ./.;};
                buildPhase = "true";
                installPhase = "mkdir $out; cp * $out/";
              };
            };
            packages = {
              fstar =
                tools.perform-fstar-to-ocaml master
                  (
                    master.overrideAttrs 
                      (o: {patches = [
                             ./patches/reflect-ranges.diff
                             ./patches/comments_of_module.diff
                           ];
                          })
                  );
            };
            apps = {
              build = pkgs.writeScriptBin "build" "${packages.fstar}/bin/fstar.exe --unsafe_tactic_exec Literate.Example.fst";
            };
            defaultPackage = packages.fstar;
          }
      );
}
