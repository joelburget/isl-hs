let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          isl-hs =
          haskellPackagesNew.callPackage ./default.nix {
            isl = pkgs.callPackage ./isl.nix { };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    isl-hs = pkgs.haskellPackages.isl-hs;
  }
