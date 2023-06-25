{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          projectRoot = ./.;
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://haskell.flake.page/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = {
          #   aeson.source = "1.5.0.0"; # Hackage version override
          #   shower.source = inputs.shower;
          # };
          settings = {
            openapi3 = {
              broken = false;
              check = false;
            };
          };

          devShell = {
            # Enabled by default
            enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              fourmolu = null;
              ghcid = null;
            };
            mkShellArgs = {
              nativeBuildInputs = [pkgs.haskell-language-server];
            };
           };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.example;
      };
    };
}
