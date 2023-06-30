{
  inputs.dream2nix.url = "github:nix-community/dream2nix";
  outputs = inputs@{self, ...}:
    (inputs.dream2nix.lib.makeFlakeOutputs {
      systems =  ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
      config.projectRoot = ./.;
      source = ./.;
      autoProjects = true;
    });
}
