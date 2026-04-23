{
  description = "Timesheet Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          hpkg = pkgs.haskellPackages;
        in f system pkgs hpkg
      );
    in {
      packages = forAllSystems (system: pkgs: hpkg:
        {
          timesheet = hpkg.developPackage {
            root = ./.;
            # This reads your cabal file and generates a derivation.
            # You don’t need to manually list attoparsec/text/random etc.
          };
        }
      );

      # `nix build` defaults to building this
      defaultPackage = forAllSystems (system: pkgs: hpkg:
        self.packages.${system}.timesheet
      );

      devShells = forAllSystems (system: pkgs: hpkg:
        {
          default = hpkg.shellFor {
            packages = p: [ self.packages.${system}.timesheet ];
            # tools for hacking:
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskell-language-server
              pkgs.ghcid
            ];
          };
        }
      );

      apps = forAllSystems (system: pkgs: hpkg:
        {
          default = {
            type = "app";
            program = "${self.packages.${system}.timesheet}/bin/timesheet";
          };
          test = {
            type = "app";
            program = toString (pkgs.writeShellScript "run-tests" ''
              echo "Running timesheet test suite..."
              echo ""
              exec ${pkgs.nix}/bin/nix develop --command ${hpkg.cabal-install}/bin/cabal test --test-show-details=streaming
            '');
          };
        }
      );
    };
}
