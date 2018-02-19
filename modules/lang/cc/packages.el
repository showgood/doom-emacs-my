;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode)
(package! demangle-mode)
(package! disaster)
(package! irony)
(package! irony-eldoc)
(package! modern-cpp-font-lock)
(package! clang-format)

(when (featurep! :feature syntax-checker)
  (package! flycheck-irony))

(when (featurep! :completion company)
  (package! company-irony)
  (package! company-irony-c-headers))

(package! rtags)
(when (featurep! :completion ivy)
  (package! ivy-rtags))
