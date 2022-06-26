(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default truncate-lines 1)

(set-fringe-mode 10)        ; Give some breathing room

;; Set up the visible bell
(setq visible-bell t)

;; so that magit does not freeze
(setq max-specpdl-size 13000)

;; set font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height 150)

(global-display-line-numbers-mode t)
(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; enable flyspell for text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Initialize package sources
(require 'package)

;; set archives to retrieve packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; get latest org mode
(use-package org :ensure t)

;; enable match indenting for org mode
(setq org-startup-indented t)

;; change text for show more
(setq org-ellipsis " ")

;; provides prettier bullits
(require 'org-superstar)

;; hook it to org-mode
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; more convienient way of setting up keybindings
(use-package general :ensure t)

;; for serching text in butter
(use-package swiper :ensure t)

;; helps with auto complete
(use-package counsel :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
	 ("C-x f" . counsel-find-file))
  )

;; enable docker file support
(use-package dockerfile-mode :ensure t)

;; hookup swiper for search
(general-define-key
  "C-s" 'swiper
  )

;(use-package hydra)

;(defhydra hydra-text-scale (:timeout 4)
;  "scale text"
;  )

;; autocomplete for search buffers
(use-package ivy
  :diminish
  :config (ivy-mode 1))

;; add extra information to search buffers
(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

;; ranbow brakets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; list menu for showing which key to use for keybinding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (which-key-mode)
  )

;; git program
(use-package magit)

;; better mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; cool color thems
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

;; show icons
(use-package all-the-icons
  :ensure t)

;; dashboard that shows up in beggining
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;(use-package dap-mode)





;;------------ this was automatically added from any custome changes made in emacs  -----------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" default))
 '(nil nil t)
 '(package-selected-packages '(dap-mode org-superstar swiper use-package ivy)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
