;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; requires node npm tern js-beautify eslint eslint-plugin-react

(package! js2-mode)
(package! js2-refactor)

(unless MINIMAL-MODE
    (package! coffee-mode)
    (package! rjsx-mode)
    (package! nodejs-repl)
    (package! tern)
    (package! skewer-mode)
    (package! eslintd-fix)
    (package! web-beautify)

(when (featurep! :completion company)
  (package! company-tern))

(when (featurep! :feature jump)
  (package! xref-js2))
  )
