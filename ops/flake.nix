{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts }: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = {pkgs, ...} :
        let
          colmena = pkgs.pkgs.colmena;
        in
        {
          apps.default = {
            type = "app";
            program = toString (pkgs.writers.writeBash "apply" ''
              ${colmena}/bin/colmena build -f network.nix
            '');
          };
          devShells.default = pkgs.mkShell {
            buildInputs = [
              colmena
            ];
          };
        };
    };
}
