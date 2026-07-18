
;; setup package
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; initialize packages
(package-initialize)

;; refresh package index
(unless package-archive-contents
  (package-refresh-contents))

;; run M-x package-refresh-contents
;; to update the index

;; Install markdown-mode
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;; Install org-mode
(unless (package-installed-p 'org)
  (package-install 'org))

;; Install doom-themes
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

;; Install emacs dashboard
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

;; Install all-the-iconsw
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))

;; Install evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Run once:
;; M-x all-the-icons-install-fonts

;; set heading icons
(setq dashboard-set-heading-icons t)

;; set file icons
(setq dashboard-set-file-icons t)

;; load dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

;; load doom-one
(load-theme 'doom-one t)

;; setup username
(setq user-full-name "Diego Vila")

;; stop making backup files
(setq make-backup-files nil)

;; take out startup screen
(setq inhibit-startup-message t)

;; cause scroll bar, tool bar, and menu to disapear
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; get ride of word wrapping
(setq-default truncate-lines 1)

;; get ride of the fringes from side of window
(set-fringe-mode 10)      

;; Set up the visible bell
;;(setq visible-bell t)

;; show line numbers
(global-display-line-numbers-mode t)

;; show column number in mode line
(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		markdown-mode-hook
                term-mode-hook
                text-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; use org mode table for markdown
(add-hook 'markdown-mode-hook 'orgtbl-mode)






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(all-the-icons dashboard doom-themes evil markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
