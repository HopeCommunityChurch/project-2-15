{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
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

        security.pki.certificates = [
          ''
            ----BEGIN CERTIFICATE-----
            MIIF8TCCA9mgAwIBAgIUCnhPPHX0m18ToSJbgjomE84GGzMwDQYJKoZIhvcNAQEL
            BQAwgYcxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJQQTEVMBMGA1UEBwwMV2lsbGlh
            bXNwb3J0MRcwFQYDVQQKDA5YdGVnbyBOZXR3b3JrczEbMBkGA1UEAwwScG9zdGFs
            Lnh0ZWdvLmNsb3VkMR4wHAYJKoZIhvcNAQkBFg9hZG1pbkB4ZXRnby5jb20wHhcN
            MjQxMjA1MTQyNjQ5WhcNMjYxMjA1MTQyNjQ5WjCBhzELMAkGA1UEBhMCVVMxCzAJ
            BgNVBAgMAlBBMRUwEwYDVQQHDAxXaWxsaWFtc3BvcnQxFzAVBgNVBAoMDlh0ZWdv
            IE5ldHdvcmtzMRswGQYDVQQDDBJwb3N0YWwueHRlZ28uY2xvdWQxHjAcBgkqhkiG
            9w0BCQEWD2FkbWluQHhldGdvLmNvbTCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCC
            AgoCggIBAOz+Rwvm+23sXxYem7h/1xJm9j1K2a8tmLEX20meAZpH9CwI2VYCkPuB
            kiDO867PIdu3hnoTB+pXt2PBSXKWyM9oZ2FA3G/w/IPDmB69BehIMvC0wwXqGctB
            M3QwMe/CwB1+XmMw/UoSrz73lNo9sZKmZlHJICEz0N7ppwlyzehBjmVJJDKxM/ar
            5571hWy3JVqlz/kCCegEAzs9qN3enPGPmumTMxdtQeuZIbeyfV4JEkGxJ7sH3vSb
            tejNYu+NLKN/VvYFh3vd0SRXUDTdJp0rQJIVFtRD69mXTg7+/ppigxfU0I3ep7CQ
            NpLoNAlRNGcXhruOiHWmbDPJ/+bgEbyi189yvxuvU5a/PfM64/uFKpHTJ8E8A5Uh
            iURhrFoWPm9ZRz1BGC2PM+0uctWO4pbIU+r3tunAIl7GzmTj9CXU6B1RPb0QVo7q
            JBoJyaLcK2Dejuhg4ZB/flSzdkRf9q4YAmp5+SKYkhFX2o8tQmsaxhYEKzpX/7NR
            e5TY0jZXyQRDn0A3Idm4w6wWaC/7k1frZtyrDFMnpRwTfdR7baiTfB/Ltv26dX5o
            51L1aYFFCfP+EwUtiAft8NQGYCBoNGz4QqwGe5FpIdJaFvFz6M/+ggc6D7WirYED
            2uztUnU0gdAdo4wePjzx7s268IGvVxZYYVWATwsCRC+lk+33aYb1AgMBAAGjUzBR
            MB0GA1UdDgQWBBTWwgjbEKObuXdfUkrOkPBEA0tIyzAfBgNVHSMEGDAWgBTWwgjb
            EKObuXdfUkrOkPBEA0tIyzAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3DQEBCwUA
            A4ICAQBi+1NpcDNlZGu29fWk7nML67UiVhnzs1aG8QYL9ah51qB5mGMzc6rSYtDd
            SsuuRfxlELRqi1d71SSNqIgtpRw/L0KnYGICjeF+/aqJTvAWIqMyyXE9DFccStGC
            UfnAFrAQOsRlw5e3NpKKK7ZWA69H1c9lUR3VdyGbal8I/VeNdYZpizPj59jqAYCi
            d3gaF/jGHMxYj1Pms4+usjVCrIk73AuZ+DnBGpLXvZojIVX7PH2x+YcTAYBgI3qZ
            DagwIyJZoTJ1r42DvqCR62V9t9Sj8BnLPcipJiFkKiuCCZD+ApSCxlQzbDhQsHpp
            acocxMUT4x6+V0iD4BV9sWOP07iKK+GX76GGTrzN2KtIOslf7SrizsCXGR/tJ+Ot
            sYQTjP6Lum8MgFDfAWG9qwb1KXiTi973DJMJzjzNFVWl/OBf9yyNgaWVMUYdF6eh
            hSwm04N2as37Gpc0zPKAKv2tPwuC9uzP344tQZczlo65HSgI7ui634BOC+ZP1xSA
            SrAhRyyfSpAEHvPIfwEImD4h+84BqjaUQ0avNZpwj/kD4Mv0gC+ksHYn9/ZISCbZ
            ytU4Ama8BL2gd/vaHGRGGV6svVIgoxl1Enq18i6SsLdIqfcytCqsIknU96K2i5Hd
            Zc6O2YRq7N7bRTVUdXGXSYDqUpkow1Yl/UzejjxvWq51e5j/KQ==
            -----END CERTIFICATE-----
          ''
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
