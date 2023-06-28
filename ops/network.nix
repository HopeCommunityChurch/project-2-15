{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/unstable";
  };
  outputs = { nixpkgs, ... }: {
    colmena = {
      meta = {
        nixpkgs = import nixpkgs {
          system = "x86_64-linux";
        };
      };

      # Also see the non-Flakes hive.nix example above.
      dev-server = {
        deployment = {
          targetHost = "146.190.186.180";
          targetPort = 22;
          targetUser = "root";
        };

        networking.firewall.allowedTCPPorts = [ 22 80 443 ];

        services.do-agent.enable = true;
        # services.nginx = {
        #   enable = true;
        #   virtualHosts.default = {
        #     default = true;
        #     locations."/".return = "200 \"Hello from Nixie!\"";
        #   };
        # };
      };
    };
  };
}
