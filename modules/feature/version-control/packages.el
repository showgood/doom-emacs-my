;; -*- no-byte-compile: t; -*-
;;; feature/version-control/packages.el

;;; config.el
;; n/a

;;; +git
(unless (featurep! -git)
  (package! git-timemachine)
  (package! evil-magit)
  (package! magit)

  (unless MINIMAL-MODE
    (package! git-gutter-fringe)
    (package! git-link)
    (package! gitconfig-mode)
    (package! gitignore-mode)
    )
  )
