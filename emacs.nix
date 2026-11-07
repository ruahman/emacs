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

        # evil
        evil
        evil-surround
        evil-commentary

        # ui
        doom-themes
        all-the-icons
        all-the-icons-dired
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
        typescript-mode

        # tree-sitter grammars (optional)
        treesit-grammars.with-all-grammars
      ];
  };

  services.emacs = {
    enable = true;
    client.enable = true; # installs a desktop entry for emacsclient
  };
}
