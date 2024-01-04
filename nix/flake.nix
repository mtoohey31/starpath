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
          version = "0.1.0";
          src = builtins.path { path = ./..; name = "starpath-src"; };
          minimalOcamlVersion = "4.14.1";
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
      inherit (pkgs) mkShell ocamlformat ocamlPackages;
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
          ocaml
          ocamlformat
          ocaml-lsp
          ounit2
          ppxlib
        ];
      };
    });
}
