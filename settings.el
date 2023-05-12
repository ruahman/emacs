(setq user-full-name "Diego Vila")

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
  (set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height 150)
  (set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height 150)
  (set-face-attribute 'default nil :font "Hack Nerd Font" :height 150))

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
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(if (eq system-type 'windows-nt)
    (progn
	(setenv "LANG" "en_US.UTF-8")
	(setq ispell-program-name "hunspell")
	(setq ispell-dictionary "en_US")
	(setq ispell-hunspell-dict-paths-alist
	  '(("en_US" "C:\\Hunspell\\en_US.aff")))))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; more convienient way of setting up keybindings
(use-package general)

;; allow repeating keys
;; (use-package hydra)

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
  (setq which-key-idle-delay 3))

;; get latest org mode
(use-package org
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-startup-indened t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-hide-emphasis-markers t)
  (setq org-ellipsis "...")
  (setq org-clock-sound "~/.emacs.d/sounds/bell3.mp3")
  (setq org-agenda-files '("~/gtd/tasks.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-refile-targets
    '(("tasks.org" :maxlevel . 1)
      ("projects.org" :maxlevel . 1)
      ("habits.org" :maxlevel . 1)
      ("someday-maybe.org" :maxlevel . 1)))

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t))))

(use-package org-contrib)

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(use-package org-drill
  :config
  (setq org-drill-learn-fraction 0.3) 
  (setq org-drill-cram-hours 0))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
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
;;(use-package magit
   ;;:custom
   ;;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/denote"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil)
  (setq denote-prompts '(title keywords))
  (setq denote-allow-multi-word-keywords t))

(use-package restclient)

(use-package rust-mode)

(use-package go-mode)

(use-package typescript-mode)

(use-package ob-rust)

(use-package ob-go)

;;(use-package ob-typescript)
;;(org-babel-do-load-languages
  ;;'org-babel-load-languages
  ;;'((typescript . t)))

(use-package ob-deno)
(add-to-list 'org-babel-load-languages '(deno . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; optional (required the typescript.el)
(add-to-list 'org-src-lang-modes '("deno" . typescript))

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

(add-hook 'dired-mode-hook #'denote-dired-mode)

;;(general-define-key
     ;;"C-x w" 'writeroom-mode)

(general-define-key
     "C-x f" 'flyspell-mode)

;;  (defhydra hydra-zoom (global-map "<f2>")
  ;;      "zoom"
  ;;      ("<up>" text-scale-increase "in")
  ;;      ("<down>" text-scale-decrease "out"))
  ;;
  ;;  (defhydra hydra-buffer (global-map "<f1>")
  ;;    "buffer"
  ;;    ("<left>" previous-buffer "prev")
  ;;    ("<right>" next-buffer "next"))
