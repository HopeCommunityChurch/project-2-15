# project-2-15

## Setup for local frontend development

1. Install nix
2. Setup nix to use [flakes](https://nixos.wiki/wiki/Flakes)
3. Install direnv
4. (optional) install [nix-direvn](https://github.com/nix-community/nix-direnv)
5. Run `devenv up`
6. Open a new tab in you terminal, and move into `frontend`
7. Run `npm run dev`

## Update backend (root)

When I update the backend and need to update the version devenv uses do:
`nix flake lock --update-input backend`

## Update front end (/frontend)

`rm -rf node_modules && rm -rf .parcel-cache && rm -rf .direnv/ && cd .. && cd frontend/`
