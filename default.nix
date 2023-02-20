let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-22.11";
    url = "https://github.com/nixos/nixpkgs/archive/3824b383c8680f0a86eefb51e843bfce3000a713.tar.gz";
    sha256 = "00jahw4wmg07ab4j90kzvq5qn3nyqfl3d8fy4ihblfr935365wdy";
  }) {};

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
      license = pkgs.lib.licenses.bsd3;
    };
in
  pkgs.haskellPackages.callPackage timesheet {}
