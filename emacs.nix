{ pkgs, ... }: {

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraConfig = builtins.readFile ./init.el;
    extraPackages =
      epkgs: with epkgs; [
        # completion
        vertico
        marginalia
        orderless
        consult
        company
        tempel

        # ui
        doom-themes
        nerd-icons
        nerd-icons-dired
        dashboard

        # org / notes
        org-superstar
        org-kanban
        org-drill
        org-roam
        consult-org-roam
        denote
        consult-denote

        # dev
        magit
        apheleia
        markdown-mode
        nix-mode
        toml-mode
        yaml-mode
        typescript-ts-mode
        js-mode
        python-mode

        # tree-sitter grammars
        treesit-grammars.with-all-grammars
        treesit-auto
      ];
  };

  services.emacs = {
    enable = true;
    client.enable = true; # installs a desktop entry for emacsclient
  };
}
