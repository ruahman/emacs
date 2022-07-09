;;(setq user-full-name "Diego Vila")

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

;; set font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height 150)
(setq default-frame-alist '((font . "Hack Nerd Font")))

;; so that magit does not freeze
(setq max-specpdl-size 13000)

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;;; Initialize package sources
(require 'package)

;; set archives to retrieve packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
;; insure that package is downloaded 
(setq use-package-always-ensure t)

;; more convienient way of setting up keybindings
(use-package general)

;; allow repeating keys
(use-package hydra)

;; for serching text in buffer
(use-package swiper
  :bind (("C-s" . swiper)))

;; autocomplete for search buffers
(use-package ivy
  :diminish
  :config (ivy-mode 1))

;; helps with auto complete
(use-package counsel :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer))
  )

;; add extra information to search buffers
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; list menu for showing which key to use for keybinding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (which-key-mode)
  )

;; get latest org mode
(use-package org
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  (setq org-startup-indented t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; (setq org-agenda-files
  ;;       '("~/Documents/org/tasks.org"
  ;;         "~/Documents/org/goals.org"
  ;;         "~/Documents/org/habits.org"
  ;;         "~/Documents/org/archive.org"))
  (setq org-ellipsis " ")
  (setq org-refile-targets '(("archive.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

;; nicer bullits for org mode
(use-package org-superstar)

(setq org-superstar-headline-bullets-list
    '("◉" "◈" "○" "▷" "⇒" "➡" "✸" "∗" "✦" "✧"))

;; setup task with pomodoros
;; (use-package org-pomodoro)
;; (
(use-package org-pomodoro
  :commands (org-pomodoro)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

(use-package org-contrib)

(use-package org-drill
  :config
  (setq org-drill-cram-hours 0))

(use-package ob-go)
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/go/bin"))
;; (
 ;; setq exec-path (append exec-path '("/usr/local/go/bin")))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (setenv "PATH" (concat (getenv "PATH") ":/home/ruahman/go/bin"))
  (setq exec-path (append exec-path '("/home/ruahman/go/bin")))
  (eshell-git-prompt-use-theme 'powerline))

;; git program
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; dashboard that shows up in beggining
(use-package dashboard
   :config
   (dashboard-setup-startup-hook))

;; cool color thems
(use-package doom-themes
  :init
  (load-theme 'doom-palenight t))

;; better mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; show icons
(use-package all-the-icons)

;; ranbow brakets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package writeroom-mode)

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
                    ;(flyspell-mode 1)
                    )))

;; hook it to org-mode
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(general-define-key
   "C-x C-d" 'org-drill)

(general-define-key
   "C-x C-k" 'org-drill-cram)

(general-define-key
   "C-x C-p" 'org-pomodoro)

(defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("<up>" text-scale-increase "in")
    ("<down>" text-scale-decrease "out"))

(defhydra hydra-buffer (global-map "<f1>")
  "buffer"
  ("<left>" previous-buffer "prev")
  ("<right>" next-buffer "next"))
