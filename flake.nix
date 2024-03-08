{
  description = "neotype - A refined newtype library for Scala 3";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
  };

  outputs = { self, nixpkgs, flake-utils, devshell }:
    
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ] (system:
      let
        pkgs =
          import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              devshell.overlays.default
            ];
          };
        helm-plugins-dir = pkgs.symlinkJoin {
          name = "helm-plugins";
          paths = with pkgs.kubernetes-helmPlugins; [
            helm-diff
            helm-secrets
          ];
        };
        jdk = pkgs.jdk11_headless;
        sbt = pkgs.sbt.override { jre = jdk; };
        metals = pkgs.metals.override { jre = jdk; };
        packages = [
          jdk
          sbt
          metals
        ];
      in
        rec {
          devShell =
            pkgs.devshell.mkShell {
              name = "neotype";
              packages = packages;
              env = [
                {
                  name = "JAVA_HOME";
                  value = "${pkgs.jdk17}";
                }
              ];
              commands = [
                {
                  name = "fmt";
                  category = "Linting";
                  help = "run scalafmt + scalafix targets";
                  command = "${pkgs.sbt}/bin/sbt ';scalafixAll;scalafmtSbt;scalafmtAll'";
                }
                {
                  name = "bloopInstall";
                  help = "runs 'sbt bloopInstall'";
                  category = "Sanity";
                  command = ''sbt bloopInstall'';
                }
              ];
            };
      }
    );
}
