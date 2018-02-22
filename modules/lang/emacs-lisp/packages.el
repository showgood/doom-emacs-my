;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! auto-compile)
(package! highlight-quoted)
(package! macrostep)
(package! overseer)
(package! slime)
(package! lispy)
(package! lispyville)

(when (featurep! :feature syntax-checker)
  (package! flycheck-cask))
