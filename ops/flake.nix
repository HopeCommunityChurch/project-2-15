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
              cd network
              ${colmena}/bin/colmena build
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
