# project-2-15

## Setup for local frontend development
1. Install nix
2. Setup nix to use [flakes](https://nixos.wiki/wiki/Flakes)
3. Install direnv
4. (optional) install [nix-direvn](https://github.com/nix-community/nix-direnv)

## Stertup each time

1. In root project directory: Run `devenv up`
2. Open a new tab in you terminal, and move into `frontend`
3. Run `npm run dev`
4. Open browser to 127.0.0.1:8080

## Update front end (/frontend) (Have to do this occasionally)
`rm -rf node_modules && rm -rf .parcel-cache && rm -rf .direnv/ && cd .. && cd frontend/`

## Branch ettiquite
1. Make changes on a new branch
2. Push changes to branch
3. Make a pull request to main
    - Change compare branch to be mine
    - Assign Jon (it should send him an email)
    - Send Jon a text the first time to confirm this is working properly
4. Jon will be prompted to review