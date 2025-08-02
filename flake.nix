{
  description = "Premiumize.me CLI tool - manage downloads, transfers and cloud storage";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        # Define the Haskell package
        premiumize-cli = haskellPackages.callCabal2nix "premiumize-cli" ./. { };

        # Development tools
        devTools = with pkgs; [
          # Haskell development
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.ormolu
          haskellPackages.hpack

          # General development tools
          git
          curl
          jq

          # Build tools
          pkg-config
          zlib
          zlib.dev
        ];

      in
      {
        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = devTools ++ [
            # Additional dependencies for development
            pkgs.which
            pkgs.findutils
          ];

          shellHook = ''
            echo "🚀 Premiumize CLI Development Environment"
            echo "📦 Available tools:"
            echo "  • ghc $(ghc --version | cut -d' ' -f8)"
            echo "  • cabal $(cabal --version | cut -d' ' -f3)"
            echo "  • hlint, ormolu, hls"
            echo ""
            echo "🔧 Quick start:"
            echo "  • cabal build               # Build the project"
            echo "  • cabal run                 # Run the CLI"
            echo "  • cabal test                # Run tests"
            echo "  • cabal repl                # Start REPL"
            echo ""
            echo "🎨 Code formatting & linting:"
            echo "  • ormolu --mode inplace --unsafe src/ app/ test/ # Format Haskell code"
            echo "  • ormolu --mode check --unsafe src/ app/ test/   # Check Haskell formatting"
            echo "  • hlint src/                                     # Lint Haskell code"
            echo "  • nix flake check                                # Run all checks"
            echo ""
            echo "📋 Don't forget to set PREMIUMIZE_API_KEY environment variable!"
          '';

          # Set up environment for Haskell development
          NIX_GHC_LIBDIR = "${haskellPackages.ghc}/lib/ghc-${haskellPackages.ghc.version}";
        };

        # The package itself
        packages = {
          default = premiumize-cli;
          premiumize-cli = premiumize-cli;
        };

        # Apps that can be run with 'nix run'
        apps = {
          default = {
            type = "app";
            program = "${premiumize-cli}/bin/premiumize-cli";
          };

          premiumize-cli = {
            type = "app";
            program = "${premiumize-cli}/bin/premiumize-cli";
          };
        };

        # Formatter for the Nix files
        formatter = pkgs.nixpkgs-fmt;

        # Checks for CI/CD
        checks = {
          build = premiumize-cli;

          # Add Nix format check
          nix-format-check = pkgs.runCommand "nix-format-check"
            {
              buildInputs = [ pkgs.nixpkgs-fmt ];
            } ''
            cd ${./.}
            nixpkgs-fmt --check flake.nix
            touch $out
          '';

          # Add Ormolu format check
          ormolu-format-check = pkgs.runCommand "ormolu-format-check"
            {
              buildInputs = [ haskellPackages.ormolu ];
            } ''
            cd ${./.}
            ormolu --mode check --unsafe $(find src app test -name "*.hs" 2>/dev/null || true)
            touch $out
          '';

          # Add hlint check
          hlint-check = pkgs.runCommand "hlint-check"
            {
              buildInputs = [ haskellPackages.hlint ];
            } ''
            cd ${./.}
            hlint src/ app/ test/
            touch $out
          '';
        };
      });

}
