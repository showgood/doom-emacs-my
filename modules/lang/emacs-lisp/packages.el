;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! auto-compile)
(package! highlight-quoted)

(when (featurep! :feature syntax-checker)
  (package! flycheck-cask))

(unless (equal doom-mode "minimal")
    (package! macrostep)
    (package! overseer)
    (package! slime)
    (package! lispy)
    (package! lispyville)
)
