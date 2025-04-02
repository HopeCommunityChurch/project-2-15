# project-2-15

## Setup for local frontend development

1. Install nix
2. Setup nix to use [flakes](https://nixos.wiki/wiki/Flakes)
3. Install direnv
4. (optional) install [nix-direvn](https://github.com/nix-community/nix-direnv)

## Stertup each time

5. Run `devenv up`
6. Open a new tab in you terminal, and move into `frontend`
7. Run `npm run dev`
8. Open browser to 127.0.0.1:8080

## Update front end (/frontend)

`rm -rf node_modules && rm -rf .parcel-cache && rm -rf .direnv/ && cd .. && cd frontend/`
