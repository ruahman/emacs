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

;;;; Initialize package sources
  ;;(require 'package)

  ;; set archives to retrieve packages
  ;;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ;;("org" . "https://orgmode.org/elpa/")
                           ;;("elpa" . "https://elpa.gnu.org/packages/")
                           ;;("nongnu" . "https://elpa.nongnu.org/nongnu/")))

  ;;(package-initialize)

  ;;(unless package-archive-contents
    ;;(package-refresh-contents))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (unless (package-installed-p 'use-package)
;;         (package-install 'use-package))

;; (require 'use-package)
;; insure that package is downloaded 
;; (setq use-package-always-ensure t)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

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
  :config
  (setq which-key-idle-delay 3))

;; get latest org mode
(use-package org
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-startup-indented t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ")
  (setq org-clock-sound "~/.emacs.d/sounds/bell3.mp3"))

;;(use-package org-contrib)

;; nicer bullits for org mode
(use-package org-superstar)

(setq org-superstar-headline-bullets-list
    '("◉" "◈" "▶" "○" "◇" "▷"))

(use-package org-roam)

(use-package org-drill
  :config
  (setq org-drill-cram-hours 0))

;; git program
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

;; (general-define-key
;;    "C-x C-d" 'org-drill)
;;
;; (general-define-key
;;    "C-x C-k" 'org-drill-cram)
;;
;; (general-define-key
;;    "C-x C-p" 'org-pomodoro)
;;  (defhydra hydra-zoom (global-map "<f2>")
;;      "zoom"
;;      ("<up>" text-scale-increase "in")
;;      ("<down>" text-scale-decrease "out"))
;;
;;  (defhydra hydra-buffer (global-map "<f1>")
;;    "buffer"
;;    ("<left>" previous-buffer "prev")
;;    ("<right>" next-buffer "next"))
