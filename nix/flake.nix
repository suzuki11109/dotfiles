{
  description = "Aki's nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }:
  let
    user = "aki";
    configuration = { pkgs, nixpkgs , ... }: {

      nixpkgs.config.allowUnfree = true;

      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages =
          [
            pkgs.git
            pkgs.neovim
            pkgs.fd
            pkgs.ripgrep
            pkgs.tree-sitter
            pkgs.colima
            pkgs.docker-client

            pkgs.emacs
            pkgs.ghostty-bin
            pkgs.stats
            pkgs.raycast
            pkgs.brave
            pkgs.dbeaver-bin
            pkgs.vscodium

            pkgs.teams
          ];

      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";

      # Enable alternative shell support in nix-darwin.
      # programs.fish.enable = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 6;

      system.primaryUser = "${user}";

      # The platform the configuration will be used on.
      nixpkgs.hostPlatform = "aarch64-darwin";

      fonts.packages = with pkgs; [
        nerd-fonts.jetbrains-mono
        nerd-fonts.symbols-only
        inter
      ];
    };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#Akis-MacBook-Air
    darwinConfigurations."Akis-MacBook-Air" = nix-darwin.lib.darwinSystem {
      modules = [
        configuration
        ./brew.nix
      ];
    };
  };
}
