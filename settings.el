;;(setq user-full-name "Diego Vila")

;; take out startup screen
;;(setq inhibit-startup-message t)

;; cause scroll bar, tool bar, and menu to disapear
;(scroll-bar-mode -1)
;(tool-bar-mode -1)
;(menu-bar-mode -1)

;; get ride of word wrapping
(setq-default truncate-lines 1)

;; get ride of the fringes from side of window
;;(set-fringe-mode 0)      

;; Set up the visible bell
(setq visible-bell t)

;; show line numbers
(global-display-line-numbers-mode t)

;; show column number in mode line
(column-number-mode)

;; set font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height 150)

;; so that magit does not freeze
(setq max-specpdl-size 13000)

;;;; Initialize package sources
(require 'package)

;; set archives to retrieve packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

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
         ("C-x b" . counsel-ibuffer)
         ("C-x f" . counsel-find-file))
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
  (setq org-startup-indented t)
  (setq org-agenda-start-with-log-mode t)
  ;; (setq org-agenda-files
  ;;       '("~/Documents/org/tasks.org"
  ;;         "~/Documents/org/goals.org"
  ;;         "~/Documents/org/habits.org"))
  (setq
   org-ellipsis " "))

;; nicer bullits for org mode
(use-package org-superstar)

;; setup task with pomodoros
(use-package org-pomodoro)

;; (use-package hydra)
;; ;; org-fc is not yet MELPA / ELPA
;; (use-package org-fc
;;   :load-path "~/.emacs.d/org-fc"
;;   :custom (org-fc-directories '("~/Documents/org/flashcards/"))
;;   :config
;;   (require 'org-fc-hydra))

(use-package org-drill)

;; git program
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; dashboard that shows up in beggining
;; (use-package dashboard
;;    :config
;;    (dashboard-setup-startup-hook))

;; cool color thems
(use-package doom-themes
  :init
  (load-theme 'doom-palenight t))

;; better mode line
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1))

;; ;; show icons
;; (use-package all-the-icons)

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
                    (writeroom-mode 1)
                    (flyspell-mode 1))))

;; hook it to org-mode
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;;(general-define-key
;;  "C-c a" 'org-agenda)
