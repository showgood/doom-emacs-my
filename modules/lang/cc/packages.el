;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! modern-cpp-font-lock)
(package! clang-format)

(when (featurep! :feature syntax-checker)
  (package! flycheck-irony))

(unless (equal doom-mode "minimal")
    (package! cmake-mode)
    (package! demangle-mode)
    (package! disaster)
)
