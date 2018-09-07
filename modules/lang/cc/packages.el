;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! modern-cpp-font-lock)
(package! clang-format)

(unless MINIMAL-MODE
    (package! cmake-mode)
    (package! demangle-mode)
    (package! disaster)
)
