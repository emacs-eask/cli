{
  description = "Development shell for Emacs + Node.js with Eask";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      # Define the systems you want to support
      systems = [
        "x86_64-linux"
        "i686-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      {
        # Generate devShells for each system
        devShells = nixpkgs.lib.genAttrs systems (system:
          let
            # Get the nixpkgs set for the current system
            pkgs = nixpkgs.legacyPackages.${system};
          in
            {
              # Define the default development shell for the system
              default = pkgs.mkShell {
                buildInputs = [
                  pkgs.emacs
                  pkgs.nodejs
                ];

                shellHook = ''
                    echo "Welcome to the Emacs development shell with Eask!"
                    npm install --include=dev
                    export PATH="$PATH:$PWD/bin"
                    eask --version
                    '';
              };
            }
        );
      };
}
