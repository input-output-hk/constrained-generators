{ inputs, system }:

let
  inherit (inputs) nixpkgs haskell-nix pre-commit-hooks;
  inherit (pkgs) lib;

  pkgs = import nixpkgs {
    inherit system;
    config = haskell-nix.config;
    overlays = [ haskell-nix.overlay ];
  };

  name = "constrained-generators";

  project = pkgs.haskell-nix.cabalProject' ({ ... }: {
    inherit name;
    src = lib.cleanSource ../.;
    compiler-nix-name = lib.mkDefault "ghc967";
    flake.variants = {
      ghc96 = { }; # Alias for the default variant
      ghc98.compiler-nix-name = "ghc984";
      ghc910.compiler-nix-name = "ghc9102";
      ghc912.compiler-nix-name = "ghc9122";
    };
  });

  flake = project.flake { };

  packages = flake.packages;

  defaultPackage = packages."${name}:lib:${name}";

  devShells = lib.attrsets.mapAttrs
    (ghcName: _: mkShell ghcName)
    project.projectVariants;

  mkShell = ghc: import ./shell.nix { inherit ghc pkgs lib project pre-commit-hooks; };

  defaultVariant =
    lib.attrsets.foldlAttrs
      (acc: k: v: if v ? "compiler-nix-name" then acc else k)
      builtins.null
      project.args.flake.variants;

in

flake // {
  packages = packages // { default = defaultPackage; };
  devShells = devShells // { default = devShells.${defaultVariant}; };
}
