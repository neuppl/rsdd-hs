# file: flake.nix
{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    rsdd.url = "github:stites/rsdd/13c28b4?dir=nix";

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { inputs', self', system, lib, config, pkgs, ... }: {
        packages = {
          default = self'.packages.rsdd-hs;
          rsdd = inputs.rsdd.packages.${system}.rsdd.overrideAttrs (old: {
            # getting a lot of thrash on the API -- might as well disable this for now.
            doCheck = false;
          });
        };
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc94.extend (
            final: prev: {
              # seems like a dirty hack since this is a rust package...
              inherit (config.packages) rsdd;
            }
          );
          # NOTE: doesn't seem to work...
          # settings.rsdd-hs = { ... }: {
          #   extraBuildDepends = [ config.packages.rsdd ];
          # };
          devShell = {
            hlsCheck.enable = false;
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;
          };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        treefmt.config = {
          projectRootFile = "flake.nix";
          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl lib:rsdd-hs --extra-lib-dirs=${config.packages.rsdd}/lib" --warnings
            '';
            category = "Primary";
          };
          run-example = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl exe:example --extra-lib-dirs=${config.packages.rsdd}/lib" --warnings -T :main
            '';
            category = "Primary";
          };
        };

        devShells.default = pkgs.mkShell {
          name = "development shell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
            config.flake-root.devShell
            config.mission-control.devShell
          ];
          nativeBuildInputs = [
            config.packages.rsdd
          ];
        };
      };
    };
}
