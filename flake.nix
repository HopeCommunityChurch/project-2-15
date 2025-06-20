{
  description = "test";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.devenv.flakeModule
      ];
      systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devenv.shells.default = {
          name = "my-project";

          imports = [
            # This is just like the imports in devenv.nix.
            # See https://devenv.sh/guides/using-with-flake-parts/#import-a-devenv-module
            # ./devenv-foo.nix
            ./nginx.nix
          ];

          # https://devenv.sh/reference/options/
          # packages = [ config.packages.default ];

          services.mailpit = {
            enable = true;
            package = inputs.nixpkgs.legacyPackages.${system}.mailpit;
            additionalArgs = [
              "--webroot"
              "/mailpit/"
            ];
          };

          services.postgres = {
            enable = true;
            package = pkgs.postgresql_15;
            createDatabase = true;
            initialDatabases = [
              { name = "p215"; }
            ];
            listen_addresses = "127.0.0.1";
            initialScript = ''
              CREATE USER p215_user with SUPERUSER password 'password';
            '';
          };

          services.nginx = {
            enable = true;
            httpConfig = ''
              proxy_redirect          off;
              proxy_connect_timeout   60s;
              proxy_send_timeout      60s;
              proxy_read_timeout      60s;
              proxy_http_version      1.1;
              # don't let clients close the keep-alive connection to upstream. See the nginx blog for details:
              # https://www.nginx.com/blog/avoiding-top-10-nginx-configuration-mistakes/#no-keepalives
              # this is a test
              proxy_set_header "Connection" "";
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_set_header X-Forwarded-Host $host;
              proxy_set_header X-Forwarded-Server $host;
              # $connection_upgrade is used for websocket proxying
              map $http_upgrade $connection_upgrade {
                default upgrade;
                \'\'      close;
              }
              client_max_body_size 10m;
              server_tokens off;

              server {
                listen 0.0.0.0:8080;
                location /mailpit/ {
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection $connection_upgrade;
                  proxy_set_header X-Real-IP $remote_addr;
                  proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                  proxy_set_header X-Forwarded-Proto $scheme;
                  proxy_set_header X-Forwarded-Host $host;
                  proxy_set_header X-Forwarded-Server $host;
                  proxy_pass http://127.0.0.1:8025;
                }

                location /api/ {
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection $connection_upgrade;
                  proxy_set_header X-Real-IP $remote_addr;
                  proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                  proxy_set_header X-Forwarded-Proto $scheme;
                  proxy_set_header X-Forwarded-Host $host;
                  proxy_set_header X-Forwarded-Server $host;
                  proxy_hide_header Last-Modified;
                  proxy_pass http://localhost:3000/;
                }

                location / {
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection $connection_upgrade;
                  proxy_set_header X-Real-IP $remote_addr;
                  proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                  proxy_set_header X-Forwarded-Proto $scheme;
                  proxy_set_header X-Forwarded-Host $host;
                  proxy_set_header X-Forwarded-Server $host;
                  proxy_hide_header Last-Modified;
                  proxy_pass http://localhost:3001/;
                }

              }
            '';
          };

          processes.backend = {
            exec = ''
              cd backend
              chmod +x run.sh
              nix develop --command sh -c "./run.sh"
            '';
          };

          # processes.frontend = {
          #   exec = ''
          #     cd frontend
          #     nix develop --command sh -c "npm run dev"
          #   '';
          # };


          processes.hoogle = {
            exec = ''
              cd backend
              nix develop --command sh -c "hoogle server --local --port 8081"
            '';
          };

        };

      };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.

      };
    };
}
