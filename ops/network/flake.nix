{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
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
          targetHost = "178.128.133.233";
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

        services.journald = {
          rateLimitBurst = 0;
          rateLimitInterval = "0";
          extraConfig = "
            SystemKeepFree=2G
          ";
        };

        services.nginx =
        let frontend = inputs.frontend.packages.x86_64-linux.frontend;
            pkgs = import nixpkgs {};
            drv = pkgs.stdenv.mkDerivation {
                    name = "frontend-drv";
                    src = ./.;
                    buildInputs = [
                      frontend
                    ];
                    installPhase = ''
                      mkdir -p $out/
                      cp -r ${frontend}/lib/node_modules/frontend/dist/* $out/
                    '';
                  };
        in {
          enable = true;
          package = nixpkgs-unstable.legacyPackages.x86_64-linux.nginxQuic;
          recommendedProxySettings = true;
          virtualHosts = {
            "dev.p215.church" = {
              forceSSL = true;
              enableACME = true;
              quic = true;
              http3_hq = true;
              locations."/" = {
                root = "${drv}/";
              };
              locations."/app/" = {
                root = "${drv}/";
                extraConfig = "rewrite ^ /index.html break;";
              };
              locations."/htmx/" = {
                proxyPass = "http://127.0.0.1:3001/";
                proxyWebsockets = true;
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
              migrationPath = ../../backend/migrations;
              templatesPath = ../../backend/templates;
              pkgs = import nixpkgs {};
              backend-drv = pkgs.stdenv.mkDerivation {
                      name = "backend-drv";
                      src = ./.;
                      buildInputs = [
                        backend
                        templatesPath
                      ];
                      installPhase = ''
                        mkdir -p $out/
                        mkdir -p $out/templates
                        cp -r ${templatesPath} $out/templates/
                        cp -r ${backend}/bin/backend $out/
                      '';
                    };
          in
            { description = "p215 backend";
              after = [ "network.target" ];
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                WorkingDirectory = "${backend-drv}/";
                ExecStart = "${backend-drv}/backend +RTS -M1G -T";
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
