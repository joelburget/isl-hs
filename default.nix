{ mkDerivation, base, c2hs, containers, inline-c, isl, stdenv
, template-haskell, gmp, pkgconfig
}:
mkDerivation {
  pname = "isl-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers inline-c template-haskell
  ];
  librarySystemDepends = [ isl gmp ];
  libraryToolDepends = [ c2hs pkgconfig ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;

  # See https://github.com/NixOS/nixpkgs/issues/18558; hardening features break
  # linking:
  hardeningDisable = [ "bindnow" ];
  # The above line fixes the normal build, but we get a similar error while
  # building haddhocks. We don't need them yet, so we just disable for now:
  doHaddock = false;
}
