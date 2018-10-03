;; -*- no-byte-compile: t; -*-
;;; tools/rotate-text/packages.el

(if DOOM-INSTALL-PACKAGE-FROM-LOCAL
    (package! rotate-text)
    (package! rotate-text :recipe (:fetcher github :repo "debug-ito/rotate-text.el"))
)
