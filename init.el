;;;;;;;;; settings

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; get ride of word wrapping
(setq-default truncate-lines 1)

;; get ride of the fringes
(set-fringe-mode 0)      

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


;;;;;;;; packages

;;;; Initialize package sources
(require 'package)

;; set archives to retrieve packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;;; seupt use-package
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
;; insure that package is downloaded 
(setq use-package-always-ensure t)

;; more convienient way of setting up keybindings
(use-package general)

;; for serching text in butter
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
  (setq org-agenda-files
	'("~/Documents/org/tasks.org"
	  "~/Documents/org/goals.org"
	  "~/Documents/org/habits.org"))
  (setq org-ellipsis " "))

;; nicer bullits for org mode
(use-package org-superstar)

;; git program
(use-package magit)

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

;; setup task with pomodoros
(use-package org-pomodoro)

;; ;(use-package org-notifications :ensure t)

;; ranbow brakets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package writeroom-mode)

;;;;;;;; keybindings

;; (general-define-key
;;  "C-c a" 'org-agenda)

;;;;;;;; hooks

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




;;------------ this was automatically added from any custome changes made in emacs  -----------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" default))
 '(nil nil t)
 '(package-selected-packages '(dap-mode org-superstar swiper use-package ivy))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
