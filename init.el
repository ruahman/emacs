
;; snipits for for adding elisp, bash and conf code blocks
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("bash" . "src bash"))
(add-to-list 'org-structure-template-alist '("conf" . "src conf"))

;load settings from org file
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))



