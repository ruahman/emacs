
;; snipits for for adding elisp, bash and conf code blocks
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("bash" . "src bash"))
(add-to-list 'org-structure-template-alist '("conf" . "src conf"))

;load settings from org file
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

(org-babel-do-load-languages 'org-babel-load-languages '((csharp . t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(spaceline-all-the-icons writeroom-mode which-key use-package rainbow-delimiters org-superstar org-pomodoro org-notifications magit ivy-rich general doom-themes doom-modeline dockerfile-mode dashboard dap-mode counsel all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
