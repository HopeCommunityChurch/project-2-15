{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    colmena.url = "github:zhaofengli/colmena/release-0.4.x";
  };

  outputs = inputs@{ self, colmena, nixpkgs, flake-parts }: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = {pkgs, ...} :
        {
          apps.default = {
            type = "app";
            program = toString (pkgs.writers.writeBash "apply" ''
              cd network \
              && ${colmena}/bin/colmena build
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
