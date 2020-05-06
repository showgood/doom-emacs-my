;; -*- no-byte-compile: t; -*-
;;; editor/format/packages.el

;; (package! format-all :pin "47d862d40a")

;; need to use this branch since I want to disable it for certain directory
(package! format-all :recipe (:host github :repo "lassik/emacs-format-all-the-code" :branch "multi-formatter"))
