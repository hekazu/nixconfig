{ config, pkgs, ... }:

{
  imports = [ <home-manager/nixos> ];

  home-manager.users.fargate = {
    home.packages = with pkgs; [
      tree
      nerdfonts
      arandr
      autorandr
      thunderbird
    ];

    home.sessionVariables = {
        EDITOR = "vim";
    };

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
        "dist-newstyle/"
        ".envrc"
        "result"
      ];
      extraConfig = {
        init.defaultBranch = "main";
        push.autoSetupRemote = true;
        branch.autoSetupRebase = "remote";
        user.signingkey = "6E5D47F6C411A1C8";
      };
    };

    programs.vim = {
      enable = true;
      plugins = with pkgs; with vimPlugins; [
        robotframework-vim
        LanguageClient-neovim
        editorconfig-vim
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

        " Return to last edit position when opening files (You want this!)
        autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

        " Add LanguageClient-neovim bindings
        let g:LanguageClient_serverCommands = { 'haskell': ['haskell-language-server-wrapper', '--lsp'] }

        nnoremap <F5> :call LanguageClient_contextMenu()<CR>
        map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
        map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
        map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
        map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
        map <Leader>lb :call LanguageClient#textDocument_references()<CR>
        map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
        map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>
      '';
    };

    services.flameshot = {
      enable = true;
      settings = {
        General = { showStartupLaunchMessage = false; };
      };
    };

    programs.home-manager.enable = true;
    home.stateVersion = "23.05";
  };
}
