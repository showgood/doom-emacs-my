;;; app/note/config.el -*- lexical-binding: t; -*-

(def-package! deft
  :config
  (setq deft-default-extension "org"
        deft-extensions '("org")
        deft-directory "~/org"
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))
        deft-text-mode 'org-mode 
        )
    (add-to-list 'evil-emacs-state-modes 'deft-mode)
  )
