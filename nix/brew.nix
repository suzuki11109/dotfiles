{ ... }: {
  homebrew = {
    enable = true;

    brews = [
      "git-flow-next"
    ];

    casks = [
      "font-sf-pro"
      "hyperkey"

      "discord"
      "gather"
      "microsoft-teams"
    ];
  };
}
