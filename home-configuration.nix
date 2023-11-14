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
      gcb = "git checkout -b";
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
      settings = {
        sudo = {
          disabled = false;
          symbol = "👮 ";
        };
      };
    };

    programs.git = {
      enable = true;
      userName = "Henri Peurasaari";
      userEmail = "henri.peurasaari@helsinki.fi";
      ignores = [
        ".direnv/"
      ];
      extraConfig = {
        init.defaultBranch = "main";
        push.autoSetupRemote = true;
        branch.autoSetupRebase = "remote";
      };
    };

    programs.vim = {
      enable = true;
      plugins = with pkgs; with vimPlugins; [
        robotframework-vim
      ];
      settings = {
        undodir = ["~" ".vim" "undodir"];
        expandtab = true;
      };
      extraConfig = ''
        set nocompatible
        set backspace=indent,eol,start
        set ruler
        set autoindent
      '';
    };

    programs.home-manager.enable = true;
    home.stateVersion = "23.05";
  };
}
