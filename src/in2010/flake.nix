{
  description = "Nix-driven build and preview for in2010 (Org → HTML)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forEachSystem =
        f:
        builtins.listToAttrs (
          map (system: {
            name = system;
            value = f system;
          }) systems
        );
    in
    {
      packages = forEachSystem (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          # Main site package
          site = pkgs.stdenvNoCC.mkDerivation {
            pname = "in2010-site";
            version = "1.0";
            src = ./.;

            buildInputs = [ pkgs.emacs-nox ];

            buildPhase = ''
              set -eu
              export HOME=$PWD   # ensure Org writes go to the build dir (not /homeless-shelter)
              emacs --batch --load publish.el --funcall org-publish-all
            '';

            installPhase = ''
              set -eu
              mkdir -p $out/in2010
              # Put all exported HTML under /in2010
              cp -r docs/* $out/in2010/ || true

              # Put assets under /in2010/assets to match href="/in2010/assets/style.css"
              mkdir -p $out/in2010/assets
              cp -r assets/* $out/in2010/assets/
            '';

            dontFixup = true;
          };

          # Make `nix build` default to building the site
          default = self.packages.${system}.site;
        }
      );

      apps = forEachSystem (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          siteOut = self.packages.${system}.site;
        in
        {
          # Serve the built output
          serve = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "serve-in2010" ''
                set -eu
                echo "Building site…"
                nix build . --no-link
                echoServing ${siteOut} at http://127.0.0.1:8000"
                cd ${siteOut}
                exec ${pkgs.python3}/bin/python -m http.server 8000
              ''
            );
          };

          # Watch and rebuild on changes
          watch = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "watch-in2010" ''
                set -eu
                WEXEC=${pkgs.watchexec}/bin/watchexec

                nix build . --no-link

                cd ${siteOut}
                ${pkgs.python3}/bin/python -m http.server 8000 &
                SERVER_PID=$!
                echo "Serving at http://127.0.0.1:8000"
                trap 'kill "$SERVER_PID" 2>/dev/null || true' EXIT INT TERM

                cd "${toString ./.}"
                exec "$WEXEC" -r -w . -- \
                  sh -c 'echo "Rebuilding…"; nix build . --no-link && echo "Rebuilt."'
              ''
            );
          };

          # Make `nix run` default to serving
          default = self.apps.${system}.serve;
        }
      );

      devShells = forEachSystem (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.emacs
              pkgs.python3
              pkgs.watchexec
            ];
          };
        }
      );
    };
}
