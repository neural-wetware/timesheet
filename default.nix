let
  pkgs = import <nixpkgs> {};

  timesheet = { mkDerivation, base, stdenv }:
    with pkgs.haskellPackages;
    mkDerivation {
      pname = "timesheet";
      version = "1.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      buildDepends = [ pkgs.cabal-install pkgs.ghc ];
      executableHaskellDepends = [ base ];
      libraryHaskellDepends = [ attoparsec text random ];
      license = stdenv.lib.licenses.bsd3;
    };
in
  pkgs.haskellPackages.callPackage timesheet {}
