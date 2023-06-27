{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts }: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = {pkgs, ...} :
        let
          morph = pkgs.pkgs.morph;
        in
        {
          apps.default = {
            type = "app";
            program = toString (pkgs.writers.writeBash "apply" ''
              ${morph}/bin/morph --version
            '');
          };
          devShells.default = pkgs.mkShell {
            buildInputs = [
              morph
            ];
          };
        };
    };
}
