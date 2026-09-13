;; -*- lexical-binding: t; -*-

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

;; Enable which-key (Built-in for Emacs 30+)
(which-key-mode 1)

;; show directory first and show hidden for dired
(setq dired-listing-switches "-alh --group-directories-first")

;; setup ispell
(setq ispell-program-name "hunspell")
(setenv "LANG" "en_US.UTF-8")
(setq ispell-dictionary "en_US")
(setq ispell-local-dictionary "en_US")
(when (eq system-type 'windows-nt)
  (setq ispell-hunspell-dict-paths-alist
        '(("en_US" "C:/Hunspell/en_US.aff"))))


;; setup highlight of line
(add-hook 'emacs-lisp-mode-hook #'hl-line-mode)
(add-hook 'elisp-mode-hook #'hl-line-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		markdown-mode-hook
                term-mode-hook
                text-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; .project in folder mark as project


;; keybind for eshell
(global-set-key (kbd "C-c e") #'eshell)

;; Setup package
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
(use-package markdown-mode
  :hook
  (markdown-mode . hl-line-mode))

;; Setup typescript-mode
(use-package typescript-mode)

;; Setup org-mode
(use-package org
  :init
  (setq org-directory "~/org/")
  ;; nicer indent for org headlines
  (setq org-startup-indented t)
  (setq org-agenda-files
        '("~/org/tasks.org"))
  ;; (setq org-todo-keywords
  ;;       '((sequence "REPEAT(r)" "PROJECT(p)" "TODO(t)" "NEXT(n)" "WAITING(w)" "BACKLOG(b)" "|" "DONE(d!)" "CANCELLED(c!)")))
  (setq org-capture-templates
        '(
	   ("i" "Inbox" entry
	    (file+headline "~/org/inbox.org" "Inbox")
	    "* %?\n  %i\n  %a"
	    :empty-lines 1)
	   
           ("s" "Spanish" entry
            (file+headline "~/org/2_areas/spanish.org" "Spanish Vocabulary")
            "* %^{Spanish word} :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Spanish
   %^{Spanish word}

** English
   %^{English translation}

** Example
   %^{Example sentence}

** Notes
   %?"
            :empty-lines 1)
           ("v" "Vocabulary" entry
            (file+headline "~/org/2_areas/vocabulary.org" "Vocabulary")
            "* %^{Word} :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Word
   %^{Word}

** Back
   %^{Definition}

** Example
   %^{Example sentence}

** Notes
   %?"
            :empty-lines 1)))
  (setq org-capture-bookmark nil) ;; disable bookmark for capture
  (setq org-refile-targets
	'(("~/org/inbox.org" :maxlevel . 1)
	  ("~/org/tasks.org" :maxlevel . 1)
	  ("~/org/1_projects/projects.org" :maxlevel . 1)))
  ;; put logs in drawer
  (setq org-log-into-drawer t)

  ;; add timestamp for done
  (setq org-log-done 'time)

  ;; respect content of header when creating a new one
  (setq org-insert-heading-respect-content t)

  :config
  ;; setup org source blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (add-to-list 'org-src-lang-modes '("typescript" . typescript))

  :bind 
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)

  :hook
  (markdown-mode . orgtbl-mode)
  (org-mode . hl-line-mode)
  (org-mode . flyspell-mode))


;; Setup org-superstar
(use-package org-superstar
  :init
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "☆" "♦"))
  (setq org-hide-leading-stars t)
  :hook
  (org-mode . org-superstar-mode))

;; use kanban boards
(use-package org-kanban
  :bind
  ("C-c k" . org-kanban/shift))

;; for space repetition 
(use-package org-drill
  :init
  ;; fix timestamp issue
  (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")))

;; Setup org-roam
(use-package org-roam
  :init
  ;; where roam looks for notes
  (setq org-roam-directory (file-truename "~/org/2_areas/kingdom/personal-study"))
  :config
  ;; keep the SQLite database in sync automatically
  (org-roam-db-autosync-mode)
  ;; how to show search results for org-roam-node-find
  (setq org-roam-node-display-template
        (concat "${title} " (propertize "${tags}" 'face 'org-tag)))
  :bind
  ;;("C-c n f" . org-roam-node-find)
  ("C-c n c" . org-roam-capture)
  ("C-c n t" . org-roam-tag-add))

;; integrate consult with roam
;; (use-package consult-org-roam
;;   :init
;;   (require 'consult-org-roam)
;;   ;; Activate the minor mode
;;   (consult-org-roam-mode 1)
;;   :custom
;;   ;; Use `ripgrep' for searching with `consult-org-roam-search'
;;   (consult-org-roam-grep-func #'consult-ripgrep)
;;   :bind
;;   ("C-c n f" . consult-org-roam-file-find)
;;   ("C-c n b" . consult-org-roam-backlinks)
;;   ("C-c n s" . consult-org-roam-search))

;; Set denote
(use-package denote
  :init
  ;; where notes live (matches your existing org-directory)
  (setq denote-directory (expand-file-name "~/org/3_resources"))
  :config
  ;; save denote automatically
  (setq denote-save-buffers 1)
  ;; (setq denote-file-type 'markdown-yaml)'
  (setq denote-prompts '(title keywords file-type))
  (setq denote-known-keywords '("meta" "tmp" "draft"))
  :bind
  ("C-c d n" . denote)
  :hook
  (dired-mode . denote-dired-mode))

;; (use-package consult-denote
;;   :config
;;   (consult-denote-mode 1)
;;   (setq consult-denote-grep-command #'consult-ripgrep)
;;   :bind
;;   ("C-c d f" . consult-denote-find)
;;   ("C-c d s" . consult-denote-grep))



;;; Tempel snippet setup -----------------------------------------------------

(use-package tempel
  :bind
  ("M-+" . tempel-complete)
  ("M-*" . tempel-insert)
  :init
  ;; Define templates inline instead of in a separate file
  (setq tempel-template-sources
        (list (lambda ()
                '(fundamental-mode

                  org-mode

                  (src "#+begin_src " (p "emacs-lisp" language) n> r> n> "#+end_src")
                  (typescript "#+begin_src typescript" n> r> n> "#+end_src")
                  (python "#+begin_src python" n> r> n> "#+end_src")))))
  ;; wires Tempel into Emacs's completion-at-point (capf) system for the current buffer
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  ;; hooks tempel to buffer
  (add-hook 'org-mode-hook 'tempel-setup-capf)
  :config
  ;; Optional: bind next/previous field navigation while a template is active
  (keymap-set tempel-map "M-n" #'tempel-next)
  (keymap-set tempel-map "M-p" #'tempel-previous))

;; add project.el
(use-package project
  :config
  (setq project-vc-extra-root-markers '(".project")))


;; Setup doom-themes
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Setup nerd-icons
(use-package nerd-icons)

;; Setup nerd-icons-dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package all-the-icons)
;; Run once:
;; M-x all-the-icons-install-fonts

;; Setup dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)  
  ;; set heading icons
  (setq dashboard-set-heading-icons t)
  ;; set file icons
  (setq dashboard-set-file-icons t))

;; Setup evil
(use-package evil
  :bind
  ("C-c v" . evil))

;; Setup magit
(use-package magit
  :bind
  ("C-c m" . magit))

;; pretier mini buffer
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;:config
  ;(setq vertico-preselect 'prompt)
  )

;; add anotations to mini buffer results
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


;; matcher for min buffer
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))


;; mini-buffer tools
(use-package consult
  :ensure t
  :bind
  ("C-s"   . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x g" . consult-ripgrep)
  ("C-x f" . consult-find)
  ("C-x r l" . consult-bookmark))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons consult-denote consult-org-roam dashboard doom-themes
		   evil magit marginalia markdown-mode
		   nerd-icons-dired orderless org-drill org-kanban
		   org-superstar projectile swiper tempel
		   typescript-mode vertico))
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
