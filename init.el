
;; setup package
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

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

(setq org-startup-indented t) ;; setup org-indent-mode

;; use org mode table for markdown
(add-hook 'markdown-mode-hook 'orgtbl-mode)

;; Install org-superstar
(unless (package-installed-p 'org-superstar)
  (package-install 'org-superstar))

(setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "♦"))
(setq org-hide-leading-stars t)

(add-hook 'org-mode-hook #'org-superstar-mode)


;; Install doom-themes
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

;; load doom-one theme
(load-theme 'doom-one t)

;; Install emacs dashboard
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

;; Install nerd-icons
(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))

;; run M-x nerd-icons-install-fonts

;; Install nerd-icons-dired
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

;; Install evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Install magit
(unless (package-installed-p 'magit)
  (package-install 'magit))


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

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		markdown-mode-hook
                term-mode-hook
                text-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;; add icons to dired
(require 'nerd-icons-dired)

(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; Enable line highlighting only in programming and text modes
;; Highlight the current line only when editing Emacs Lisp files
(add-hook 'emacs-lisp-mode-hook #'hl-line-mode)(add-hook 'elisp-mode-hook #'hl-line-mode)
(add-hook 'markdown-mode-hook #'hl-line-mode)


(defun zettle ()
  "Create a new Zettelkasten note in either Markdown or Org format.
Prompts for note type, title, and comma-separated tags, then creates
the file with proper front matter and includes tags in the filename."
  (interactive)
  (let* ((type (completing-read "Note format: " '("org" "markdown") nil t))
         (title (read-string "Title: "))
         (tags-raw (read-string "Tags (comma separated): "))
         ;; Clean up tags into a clean list of trimmed strings
         (tags (mapcar #'string-trim (split-string tags-raw "," t)))
         
         ;; Create safe slugs for title and tags (lowercase, alphanumeric + hyphens)
         (title-slug (string-trim (downcase (replace-regexp-in-string "[^A-Za-z0-9]+" "-" title)) "-" "-"))
         (tags-slug (string-trim (downcase (replace-regexp-in-string "[^A-Za-z0-9]+" "-" (mapconcat #'identity tags "-"))) "-" "-"))
         
         ;; Generate timing strings
         (datetime (format-time-string "%Y%m%d%H%M%S"))
         (display-date (format-time-string "%Y-%m-%d %H:%M"))
         
         ;; Build filename: <datetime>_<title>_<tags>.<ext>
         (ext (if (string= type "markdown") "md" "org"))
         (filename (if (string-empty-p tags-slug)
                       (format "%s__%s.%s" datetime title-slug ext)
                     (format "%s__%s__%s.%s" datetime title-slug tags-slug ext))))
    
    ;; Open the new file buffer
    (find-file filename)
    
    ;; Insert the appropriate front matter based on chosen format
    (cond
     ((string= type "markdown")
      (insert "---\n")
      (insert (format "title: \"%s\"\n" title))
      (insert (format "date: %s\n" display-date))
      (insert (format "tags: [%s]\n" (mapconcat (lambda (s) (format "\"%s\"" s)) tags ", ")))
      (insert "---\n\n"))
     
     ((string= type "org")
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE:  %s\n" display-date))
      (insert (format "#+FILETAGS: %s\n" (mapconcat #'identity tags " ")))
      (insert "\n")))
    
    (message "Zettel note created: %s" filename)))


(defun zettle-update ()
  "Update current buffer's filename based on title and tags updated in front matter.
Maintains the existing original 14-digit datetime prefix."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  
  (let* ((current-path (buffer-file-name))
         (current-name (file-name-nondirectory current-path))
         (dir (file-name-directory current-path))
         (ext (file-name-extension current-name))
         
         ;; Pull the original datetime prefix
         (parts (split-string (file-name-base current-name) "__"))
         (datetime (nth 0 parts))
         parsed-title
         tags)
    
    ;; Guard check for the 14-digit timestamp
    (unless (and datetime (= (length datetime) 14))
      (user-error "Filename doesn't start with a valid YYYYMMDDHHMMSS timestamp"))
    
    ;; 1. Parse BOTH title and tags out of the buffer front matter
    (save-excursion
      (goto-char (point-min))
      (cond
       ;; --- MARKDOWN PARSING ---
       ((string= ext "md")
        ;; Parse title
        (if (re-search-forward "^title:\\s-*\"\\(.*\\)\"" nil t)
            (setq parsed-title (match-string 1))
          (if (re-search-forward "^title:\\s-*\\(.*\\)$" nil t)
              (setq parsed-title (match-string 1))))
        ;; Parse tags
        (if (re-search-forward "^tags:\\s-*\\[\\(.*\\)\\]" nil t)
            (let ((raw-tags (match-string 1)))
              (setq tags (mapcar (lambda (s) (string-trim (replace-regexp-in-string "\"" "" s)))
                                 (split-string raw-tags "," t))))))
       
       ;; --- ORG MODE PARSING ---
       ((string= ext "org")
        ;; Parse title
        (if (re-search-forward "^#\\+TITLE:\\s-*\\(.*\\)$" nil t)
            (setq parsed-title (match-string 1)))
        ;; Parse tags
        (if (re-search-forward "^#\\+FILETAGS:\\s-*\\(.*\\)$" nil t)
            (setq tags (split-string (match-string 1) "\\s-+" t))))))
    
    ;; Fall back to existing name if no title found in front matter
    (unless parsed-title
      (user-error "Could not find a valid title line in the front matter"))
    
    ;; 2. Re-slugify both fields cleanly
    (let* ((title-slug (string-trim (downcase (replace-regexp-in-string "[^A-Za-z0-9]+" "-" parsed-title)) "-" "-"))
           (tags-slug (if tags
                          (string-trim (downcase (replace-regexp-in-string "[^A-Za-z0-9]+" "-" (mapconcat #'identity tags "-"))) "-" "-")
                        ""))
           ;; Assemble the modern name using the double underscores
           (new-name (if (string-empty-p tags-slug)
                         (format "%s__%s.%s" datetime title-slug ext)
                       (format "%s__%s__%s.%s" datetime title-slug tags-slug ext)))
           (new-path (expand-file-name new-name dir)))
      
      ;; 3. Execute renaming sequence if something actually changed
      (if (string= current-name new-name)
          (message "Filename is already completely up to date.")
        
        (when (buffer-modified-p) (save-buffer))
        
        (rename-file current-path new-path 1)
        (set-visited-file-name new-path)
        (set-buffer-modified-p nil)
        (message "Renamed Zettel to: %s" new-name)))))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
