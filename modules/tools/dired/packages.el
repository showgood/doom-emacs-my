;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! dired-k)
(package! stripe-buffer)
(package! dired+ :recipe (:fetcher github :repo "emacsmirror/dired-plus"))
(package! dired-hacks-utils)

(unless (equal doom-mode "minimal")
    (package! ranger)
    (package! peep-dired)
    (package! dired-sidebar)
    (package! dired-quick-sort)
    (package! all-the-icons-dired))
