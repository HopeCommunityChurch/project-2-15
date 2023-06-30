{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    backend.url = "path:../../backend/";
    frontend.url = "path:../../frontend/";
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

        networking.firewall.allowedTCPPorts = [ 22 80 443 ];

        services.do-agent.enable = true;

        services.sshd = {
          enable = true;
        };

        services.nginx = {
          enable = true;
          recommendedProxySettings = true;
          virtualHosts = {
            "dev.p215.church" = {
              forceSSL = true;
              enableACME = true;
              locations."/" = {
                let frontend = inputs.frontend.packages.x86_64-linux.frontend;
                 in root = "${frontend}/lib/node_modules/frontend/dist/";
              };
              locations."/api/" = {
                proxyPass = "http://127.0.0.1:3000/";
                proxyWebsockets = true;
              };
            };
          };
          eventsConfig = ''
            worker_connections 20000;
          '';
        };

        security.acme.acceptTerms = true;
        security.acme.certs."dev.p215.church" = {
          email = "jonny.covert@gmail.com";
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
