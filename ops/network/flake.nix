{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    backend.url = "path:../../backend/";
    frontend.url = "path:../../frontend/";
  };
  outputs = inputs@{ nixpkgs, nixpkgs-unstable, ... }: {
    colmena = {
      meta = {
        nixpkgs = import nixpkgs {
          system = "x86_64-linux";
        };
      };

      defaults = { pkgs, ... }: {
        # This module will be imported by all hosts
        environment.systemPackages = with pkgs; [
          neovim wget curl fish
        ];
        nix.gc = {
          automatic = true;
        };
      };

      # Also see the non-Flakes hive.nix example above.
      dev-server = {
        deployment = {
          targetHost = builtins.getEnv "SERVER_IP";
          targetUser = "root";
          keys."secrets" =
            let fileLoc = builtins.getEnv "SECRETS_FILE_LOC";
            in {
            keyFile = "${fileLoc}";
          };
        };
        boot.isContainer = true;

        networking.firewall = {
          allowedTCPPorts = [ 22 80 443 ];
          allowedUDPPorts = [ 443 ];
          rejectPackets = false;
        };

        services.do-agent.enable = true;

        services.sshd = {
          enable = true;
        };

        services.nginx = let host = builtins.getEnv "HOST_NAME" in {
          enable = true;
          package = nixpkgs-unstable.legacyPackages.x86_64-linux.nginxQuic;
          recommendedProxySettings = true;
          virtualHosts = {
            "${host}" = {
              forceSSL = true;
              enableACME = true;
              quic = true;
              http3_hq = true;
              locations."/" =
                let frontend = inputs.frontend.packages.x86_64-linux.frontend;
                in {
                root = "${frontend}/lib/node_modules/frontend/dist/";
              };
              locations."/app/" =
                let frontend = inputs.frontend.packages.x86_64-linux.frontend;
                in {
                root = "${frontend}/lib/node_modules/frontend/dist/";
                extraConfig = "rewrite ^ /index.html break;";
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
        security.acme.certs."${host}"  = {
          email = "jonny.covert@gmail.com";
        };

        systemd.services.backend =
          let backend = inputs.backend.packages.x86_64-linux.backend;
              migrationPath = ../../backend/migrations;
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
                Environment = [
                  "MIGRATION_PATH=${migrationPath}"
                  "SECRETS_FILE=/var/run/keys/secrets"
                ];
              };
            };


      };
    };
  };
}
