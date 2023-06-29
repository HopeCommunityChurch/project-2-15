{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    backend.url = "path:../../backend/";
  };
  outputs = inputs@{ nixpkgs, ... }: {
    colmena = {
      meta = {
        nixpkgs = import nixpkgs {
          system = "x86_64-linux";
        };
      };

      # Also see the non-Flakes hive.nix example above.
      dev-server = {
        deployment = {
          targetHost = "178.128.133.233";
          targetUser = "root";
        };
        boot.isContainer = true;

        networking.firewall.allowedTCPPorts = [ 22 80 443 3000 ];

        services.do-agent.enable = true;

        services.sshd = {
          enable = true;
        };

        services.nginx = {
          enable = true;
          virtualHosts.default = {
            default = true;
            locations."/".return = "200 \"Hello from Nixie!\"";
          };
        };

        systemd.services.backend =
          let backend = inputs.backend.packages.x86_64-linux.backend;
          in
            { description = "p215 backend";
              after = [ "network.target" ];
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                WorkingDirectory = "${backend}/bin/";
                ExecStart = "${backend}/bin/backend +RTS -M1G -T";
                Restart = "always";
                RestartSec = 3;
                LimitNOFILE = 65536;
                Environment = [  ]; # env variables here
              };
            };


      };
    };
  };
}
