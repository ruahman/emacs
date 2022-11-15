
;; load settings from org file
(require 'org)
(org-babel-load-file
  (expand-file-name "settings.org"
                    user-emacs-directory))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-roam yasnippet writeroom-mode which-key use-package spaceline-all-the-icons rust-mode restclient rainbow-delimiters org-superstar org-pomodoro org-drill org-contrib ob-swiftui ob-swift ob-rust ob-go magit ivy-rich hydra general eshell-git-prompt doom-themes doom-modeline dashboard counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
