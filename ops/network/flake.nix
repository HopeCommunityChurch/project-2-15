{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.05";
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

      p215-server = let host = builtins.getEnv "HOST_NAME"; in {
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

        security.pki.certificateFiles = [
          (builtins.readFile ../email.cert)
        ];

        networking.firewall = {
          allowedTCPPorts = [ 22 80 443 ];
          allowedUDPPorts = [ 443 ];
          rejectPackets = false;
        };

        services.do-agent.enable = true;

        services.postfix = {
          enable = true;
          origin = host;
          hostname = host;
          enableSmtp = true;
        };

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

        services.nginx = {
          enable = true;
          package = nixpkgs-unstable.legacyPackages.x86_64-linux.nginxQuic;
          recommendedProxySettings = true;
          virtualHosts = {
            "${host}" = {
              forceSSL = true;
              enableACME = true;
              quic = true;
              http3_hq = true;
              locations."/" = {
                proxyPass = "http://127.0.0.1:3001/";
                proxyWebsockets = true;
                extraConfig = ''
                  proxy_hide_header Last-Modified;
                '';
              };
              locations."/app/" = {
                proxyPass = "http://127.0.0.1:3001/";
                proxyWebsockets = true;
                extraConfig = ''
                  proxy_hide_header Last-Modified;
                '';
              };
              locations."/api/" = {
                proxyPass = "http://127.0.0.1:3000/";
                proxyWebsockets = true;
                extraConfig = ''
                  proxy_hide_header Last-Modified;
                '';
              };
            };
          };
          eventsConfig = ''
            worker_connections 20000;
          '';
        };

        system.stateVersion = "23.11";

        security.acme.acceptTerms = true;
        security.acme.certs."${host}"  = {
          email = "jonny.covert@gmail.com";
        };

        systemd.services.backend =
          let backend = inputs.backend.packages.x86_64-linux.backend;
              migrationPath = ../../backend/migrations;
              templatesPath = ../../backend/templates;
              staticPath = ../../backend/static;
              frontend = inputs.frontend.packages.x86_64-linux.frontend;
              pkgs = import nixpkgs {};
              backend-drv = pkgs.stdenv.mkDerivation {
                      name = "backend-drv";
                      src = ./.;
                      buildInputs = [
                        backend
                        templatesPath
                        frontend
                      ];
                      installPhase = ''
                        mkdir -p $out/
                        mkdir -p $out/templates
                        mkdir -p $out/static
                        mkdir -p $out/static/editor
                        cp -r ${templatesPath}/* $out/templates/
                        cp -r ${staticPath}/* $out/static/
                        cp -r ${backend}/bin/backend $out/
                        cp -r ${frontend}/lib/node_modules/frontend/dist/* $out/static/editor/
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
