;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! modern-cpp-font-lock)
(package! clang-format)

(when (featurep! :feature lsp)
    (package! ccls))

(unless MINIMAL-MODE
    (package! cmake-mode)
    (package! demangle-mode)
    (package! disaster)
)
