{
  description = "Nix-driven build and preview for Org → HTML";
  # This file is mainly GPT generated as a fun test of how current models work
  # I could've highly probably build a better/faster implementation, but
  # decided to spend 4 hours arguing with different GPT models

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forEachSystem =
        f:
        lib.genAttrs systems (
          system:
          let
            pkgs = import nixpkgs { inherit system; };
            emacs = pkgs.emacs-nox;
          in
          f { inherit pkgs emacs system; }
        );
    in
    {
      packages = forEachSystem (
        { pkgs, emacs, ... }:
        let
          site = pkgs.stdenvNoCC.mkDerivation {
            pname = "site";
            version = "1.0";
            src = ./.;

            buildInputs = [ emacs ];

            buildPhase = ''
              set -euo pipefail
              export HOME="$PWD"
              export LC_ALL=C.UTF-8
              mkdir -p "public"

              ${emacs}/bin/emacs -Q --batch \
                --load publish.el \
                --funcall org-publish-all
            '';

            installPhase = ''
              set -euo pipefail
              mkdir -p "$out"
              cp -r "public/." "$out/"
            '';
          };
        in
        {
          site = site;
          default = site;
        }
      );

      apps = forEachSystem (
        { pkgs, system, ... }:
        {
          serve = {
            type = "app";
            program = "${
              pkgs.writeShellApplication {
                name = "serve";
                runtimeInputs = [
                  pkgs.nix
                  pkgs.python3
                ];
                text = ''
                  set -euo pipefail
                  # Always rebuild to pick up changes
                  out=$(nix build .#site --no-link --print-out-paths)

                  echo "Serving at http://127.0.0.1:8000"
                  cd "$out"
                  exec python -m http.server 8000
                '';
              }
            }/bin/serve";
          };

          default = self.apps.${system}.serve;
        }
      );
    };
}
