{ config, pkgs, ... }:

{
  imports = [ <home-manager/nixos> ];

  home-manager.users.fargate = {
    home.packages = with pkgs; [
      tree
      nerdfonts
      arandr
    ];

    home.shellAliases = {
      ga = "git add";
      gs = "git status";
      gco = "git commit";
      gp = "git push";
      gib = "git pull";
    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableAutosuggestions = true;
    };

    programs.alacritty = {
      enable = true;
      settings = {
        font = { size = 8.0; };
      };
    };

    programs.starship = {
      enable = true;
      enableZshIntegration = true;
    };

    programs.git = {
      enable = true;
      userName = "Henri Peurasaari";
      userEmail = "henri.peurasaari@helsinki.fi";
      ignores = [
        ".direnv/"
      ];
    };

    programs.home-manager.enable = true;
    home.stateVersion = "23.05";
  };
}
