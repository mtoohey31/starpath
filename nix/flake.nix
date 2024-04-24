{
  description = "starpath";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: {
    overlays.default = _: prev: {
      ocamlPackages = prev.ocamlPackages.overrideScope' (final: _: {
        starpath = final.buildDunePackage {
          pname = "starpath";
          version = "0.1.3";
          src = builtins.path { path = ./..; name = "starpath-src"; };
          minimalOcamlVersion = "4.11.2";
          duneVersion = "3";
          checkInputs = [ final.ounit2 ];
        };
      });
    };
  } // utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        overlays = [ self.overlays.default ];
        inherit system;
      };
      inherit (pkgs) gmp mkShell ocamlformat ocamlPackages opam openssl
        pkg-config;
      inherit (ocamlPackages) bisect_ppx dune_3 findlib ocaml ocaml-lsp ounit2
        ppxlib starpath;
    in
    {
      packages.default = starpath;

      devShells.default = mkShell {
        packages = [
          bisect_ppx
          dune_3
          findlib
          gmp.dev
          ocaml
          ocamlformat
          ocaml-lsp
          opam
          openssl.dev
          ounit2
          pkg-config
          ppxlib
        ];
      };
    });
}
