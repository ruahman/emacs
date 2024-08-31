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
(setq visible-bell t)

;; show line numbers
(global-display-line-numbers-mode t)

;; show column number in mode line
(column-number-mode)

;; set font for client
(defun set-font-faces()
  (message "setting font")
  (set-face-attribute 'variable-pitch nil :font "FiraCode Nerd Font" :height 150)
  (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 150)
  (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 150))

;; if emacs is running as a server
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-font-faces))))
    (set-font-faces))

;; so that magit does not freeze
(setq max-specpdl-size 13000)

;; initial buffer to show when in emacsclient
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(if (eq system-type 'windows-nt)
    (progn
      (setenv "LANG" "en_US.UTF-8")
      (setq ispell-program-name "hunspell")
      (setq ispell-dictionary "en_US")
      (setq ispell-hunspell-dict-paths-alist
        '(("en_US" "C:\\Hunspell\\en_US.aff")))
      (setq pandoc "c:/users/dego_/appdata/local/pandoc/pandoc.exe")))

(if (eq system-type 'gnu/linux) 
    (progn
      (setq pandoc "/usr/bin/pandoc")))

(if (eq system-type 'darwin)
    (progn
      (setq pandoc "/opt/homebrew/bin/pandoc")))

;; so that split is always horizontal
(setq split-width-threshold 4000)

;; Set find-name-dired to use case-insensitive search
(setq find-name-arg "-iname")

(setq dictionary-server "dict.org")

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; more convienient way of setting up keybindings
(use-package general)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command pandoc)
  (setq markdown-fontify-code-blocks-natively t))

;; for serching text in buffer
(use-package swiper
  :bind (("C-s" . swiper)))

;; autocomplete for buffers
(use-package ivy
   :diminish
   :config (ivy-mode 1))

;; add some functions you can add that uses ivy
(use-package counsel :ensure t
   :bind (("M-x" . counsel-M-x)
          ("C-x b" . counsel-ibuffer)))

;; add extra information to search buffers
(use-package ivy-rich
   :init
   (ivy-rich-mode 1))

;; list menu for showing which key to use for keybinding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 5))

;; get latest org mode
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-agenda-list))
  :config
  ;;(setq org-agenda-start-with-log-mode t)
  (setq org-agenda-start-with-follow-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-startup-indented t) ;; setup org-indent-mode
  ;;(setq org-hide-emphasis-markers t)
  (setq org-ellipsis "...")
  (setq org-clock-sound "~/.emacs.d/sounds/bell3.mp3")
  (setq org-agenda-files (list "~/gtd/tasks.org" "~/gtd/repeat.org" "~/gtd/habits.org"))

  (add-hook 'auto-save-hook 'org-save-all-org-buffers)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 70)

  ;; setup refile
  (setq org-refile-targets
    '(("tasks.org" :maxlevel . 1)
      ("repeat.org" :maxlevel . 1)
      ("habits.org" :maxlevel . 1)
      ("someday-maybe.org" :maxlevel . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)))

  (setq org-capture-templates
        '(("t" "Todo" entry
           (file "~/gtd/tasks.org")
	    (file "~/.emacs.d/tpl-todo.org"))
	   ("e" "Español" entry
              (file "~/drill/espanol.org")
   	    (file "~/.emacs.d/tpl-espanol.org"))
          ("b" "Bible" entry
           (file "~/drill/bible.org")
	    (file "~/.emacs.d/tpl-bible.org"))
	     ("d" "Denote" plain
           (file denote-last-path)
           #'denote-org-capture
           :no-save t
           :immediate-finish nil
           :kill-buffer t
           :jump-to-captured t))))

(use-package org-contrib)

(use-package org-bullets
	:hook
	(org-mode . org-bullets-mode)
	:custom
	(org-bullets-bullet-list '("○" "◎" "◉" "●" "◆" "◈" "◇")))

(use-package dired-preview
  :config
  (setq dired-preview-delay 0.2))

(use-package org-drill
  :bind
  (("C-c D" . org-drill)
   ("C-c d" . org-drill-resume))
  :config
  (setq org-drill-maximum-items-per-session 20)
  (setq org-drill-maximum-duration 10)
  (setq org-drill-learn-fraction 0.9)
  (setq org-drill-overdue-interval-factor 1.0)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-skip-easy-drill-headings t)
  (setq org-drill-cram-hours 0))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;(define-key yas-minor-mode-map (kbd "M-z") 'yas-expand)
  ;(define-key yas-keymap (kbd "M-j") 'yas-next-field-or-maybe-expand)
  ;(define-key yas-keymap (kbd "M-k") 'yas-prev-field)
  (yas-global-mode 1))

;; dashboard that shows up in beggining
(use-package dashboard
   :config
   (dashboard-setup-startup-hook))

;; cool color thems
(use-package doom-themes
  :init
  (load-theme 'doom-palenight t))

;; better mode line
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1))

;;show icons
(use-package all-the-icons)

;; ranbow brakets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package writeroom-mode)

;; git program
(use-package magit
   :custom
   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil
   :init
   (setq evil-want-integration t)
   (setq evil-want-keybinding nil)
   :bind
   (("C-c e" . evil-local-mode))
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
)

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

(use-package denote
  :config
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (setq denote-directory (expand-file-name "~/denote"))
  (setq denote-journal-extras-directory (expand-file-name "~/journal"))
  (setq denote-infer-keywords t)
  (setq denote-known-keywords '("meta" "idea", "tmp", "ref"))
  (setq denote-sort-keywords t)
  (setq denote-file-type 'markdown-yaml)
  (setq denote-prompts '(title keywords file-type signature))
  (setq denote-allow-multi-word-keywords t))

(use-package rust-mode)

(use-package ledger-mode)

(use-package go-mode)

(use-package typescript-mode)

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package zig-mode)

(use-package json-mode)

(use-package ob-rust)

(use-package ob-go)

(use-package ob-typescript)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((js . t)
    (rust . t)
    (go . t)
    (python . t)
    (typescript . t)))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                text-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; enable flyspell for text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                    (visual-line-mode 1)
                    ;(writeroom-mode 1)
                    (flyspell-mode 1)
                    )))

(add-hook 'dired-mode-hook #'denote-dired-mode)

;; for searching content in files
  (general-define-key
         "C-c r" 'rgrep)

;; for searching file names
(general-define-key
         "C-c f" 'find-name-dired)

      ;;(general-define-key
        ;; "C-c l" 'org-agenda-list)

      ;;(general-define-key
           ;;"C-x w" 'writeroom-mode)

      ;;(general-define-key
      ;;     "C-x f" 'flyspell-mode)
