
;; setup package
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")
	("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; run M-x package-refresh-contents
;; to update the index

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; load use-package
(require 'use-package)
;; install package if not found
(setq use-package-always-ensure t) 

;; Setup  markdown-mode
(use-package markdown-mode)

;; Setup typescript-mode
(use-package typescript-mode)

;; Setup org-mode
(use-package org
  :init
  (setq org-directory "~/org/")

  :config
  ;; nicer indent for org headlines
  (setq org-startup-indented t)
  (setq org-agenda-files
        (directory-files-recursively org-directory "\\.org$"))
  (setq org-todo-keywords
        '((sequence "REPEAT(r)" "PROJECT(p)" "TODO(t)" "NEXT(n)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c!)")))
  (setq org-capture-templates
        '(
	   ("i" "Inbox" entry
	    (file+headline "~/org/1_inbox/inbox.org" "inbox")
	    "* %?\n  %i\n  %a")
	   ("t" "Task" entry
	    (file+headline "~/org/2_tasks/tasks.org" "tasks")
	    "* TODO %?\n  %i\n  %a")
	   ("p" "Project" entry
	    (file+headline "~/org/3_projects/projects.org" "projects")
	    "* PROJECT %?\n  %i\n  %a")))
  (setq org-capture-bookmark nil) ;; disable bookmark for capture
  (setq org-refile-targets
	'(("~/org/1_inbox/inbox.org" :maxlevel . 1)
	  ("~/org/2_tasks/tasks.org" :maxlevel . 1)
	  ("~/org/3_projects/projects.org" :maxlevel . 1)))
  ;; put logs in drawer
  (setq org-log-into-drawer t)

  ;; add timestamp for done
  (setq org-log-done 'time)

  ;; respect content of header when creating a new one
  (setq org-insert-heading-respect-content t)

  ;; setup org source blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (add-to-list 'org-src-lang-modes '("typescript" . typescript))

  :bind 
  ("C-c a" . org-agenda)
  ("C-c l" . org-agenda-list)
  ("C-c c" . org-capture)

  :hook (markdown-mode . orgtbl-mode))

;; Setup org-superstar
(use-package org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "☆" "♦"))
  (setq org-hide-leading-stars t)
  :hook
  (org-mode . org-superstar-mode))


;;;; Install org-roam
(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))

;; where roam looks for notes (file-truename handles symlinks)
(setq org-roam-directory (file-truename "~/org/4_areas/kingdom/personal-study"))

;; keep the SQLite database in sync automatically
(org-roam-db-autosync-mode)

;; keybindings (C-c r prefix, which-key will show these)
(global-set-key (kbd "C-c r f") #'org-roam-node-find)
;; (global-set-key (kbd "C-c r i") #'org-roam-node-insert)
;; (global-set-key (kbd "C-c r t") #'org-roam-buffer-toggle) ; show backlinks
;; (global-set-key (kbd "C-c r c") #'org-roam-capture) ; use caputere to take a note

;; how to show search results for org-roam-node-find
(setq org-roam-node-display-template
      (concat "${title} " (propertize "${tags}" 'face 'org-tag)))

;;;; Install denote
(unless (package-installed-p 'denote)
  (package-install 'denote))

;; where notes live (matches your existing org-directory)
(setq denote-directory (expand-file-name "~/org/5_resources"))

;; save denote automatically
(setq denote-save-buffers 1)

;; (setq denote-file-type 'markdown-yaml)'
(setq denote-prompts '(title keywords file-type))

(setq denote-known-keywords '("meta" "tmp" "draft"))

;; (global-set-key (kbd "C-c d n") #'denote)                 ; new note
;; (global-set-key (kbd "C-c d r") #'denote-rename-file)     ; rename to denote scheme
;; (global-set-key (kbd "C-c d l") #'denote-link)            ; insert link to a note
;; (global-set-key (kbd "C-c d b") #'denote-backlinks)       ; show backlinks
;; (global-set-key (kbd "C-c d o") #'denote-open-or-create)  ; jump to / create note
;; (global-set-key (kbd "C-c d k") #'denote-rename-file-keywords) ; edit keywords

;; fontify the filename fields (date / title / keywords) in dired
(add-hook 'dired-mode-hook #'denote-dired-mode)


;;;; Install doom-themes
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

;; load doom-one theme
(load-theme 'doom-one t)

;;;; Install emacs dashboard
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

;;;; Install nerd-icons
(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))

;; run M-x nerd-icons-install-fonts

;;;; Install nerd-icons-dired
(unless (package-installed-p 'nerd-icons-dired)
  (package-install 'nerd-icons-dired))

;; load dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

;; set heading icons
(setq dashboard-set-heading-icons t)

;; set file icons
(setq dashboard-set-file-icons t)

;; Install all-the-iconsw
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))

;; Run once:
;; M-x all-the-icons-install-fonts

;;;; Install evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;;;; Install magit
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Install swiper
;;(unless (package-installed-p 'swiper)
;;  (package-install 'swiper))

;;(global-set-key (kbd "C-s") 'swiper)
;;(define-key minibuffer-local-map (kbd "C-n") 'next-line)
;;(define-key minibuffer-local-map (kbd "C-p") 'previous-line)


;; setup username
(setq user-full-name "Diego Vila")

;; set font
(set-face-attribute 'default nil
                    :font "Hack Nerd Font"
                    :height 140)

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

;; Set up the visible bell, flashes
(setq visible-bell t)

;; show line numbers
(global-display-line-numbers-mode t)

;; show column number in mode line
(column-number-mode)

;; enable fido
(fido-vertical-mode 1)

;; Enable which-key (Built-in for Emacs 30+)
(which-key-mode 1)

;; dired
(setq dired-listing-switches "-alh --group-directories-first")

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		markdown-mode-hook
                term-mode-hook
                text-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(setq ispell-program-name "hunspell")

(setq ispell-dictionary "en_US")
(setenv "LANG" "en_US")



;; add icons to dired
(require 'nerd-icons-dired)

(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; Enable line highlighting only in programming and text modes
;; Highlight the current line only when editing Emacs Lisp files
(add-hook 'emacs-lisp-mode-hook #'hl-line-mode)(add-hook 'elisp-mode-hook #'hl-line-mode)
(add-hook 'markdown-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'flyspell-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((eval setq-local org-roam-db-location
	   (expand-file-name "org-roam.db" org-roam-directory))
     (eval setq-local org-roam-directory
	   (expand-file-name
	    (locate-dominating-file default-directory ".dir-locals.el"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
