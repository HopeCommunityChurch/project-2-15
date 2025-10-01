{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-25.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    colmena.url = "github:zhaofengli/colmena";
  };

  outputs = inputs@{ self, colmena, nixpkgs, flake-parts }: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = {pkgs, system, ...} :
        let colmena-pkg = colmena.defaultPackage.${system};
        in
        {
          apps.default = {
            type = "app";
            program = toString (pkgs.writers.writeBash "apply" ''
              cd network \
              && ${colmena-pkg}/bin/colmena apply --impure
            '');
          };
          devShells.default = pkgs.mkShell {
            buildInputs = [
              colmena-pkg
            ];
          };
        };
    };
}
