# file: flake.nix
{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    haskell-flake.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    rsdd.url = "github:stites/rsdd/3ff8de49925dd1d626f689e4e0e68e66e95a478d?dir=nix";
    #nixpkgs.follows = "rsdd/nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    #rsdd.url = "path:./rsdd/nix";
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
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ (_: _: { rsdd = inputs.rsdd.packages.${system}.default.override { inherit (pkgs) rustPlatform; }; }) ];
        };
        packages = {
          default = self'.packages.rsdd-hs;
          inherit (pkgs) rsdd;
        };

        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc94;
          packages = {};
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
              ghcid -c "cabal repl lib:rsdd-hs --extra-lib-dirs=${pkgs.rsdd}/lib" --warnings
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
          nativeBuildInputs = with pkgs; [
            rsdd
          ];
        };
    };
    };
}
