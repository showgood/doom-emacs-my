;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! dired-k)
(package! stripe-buffer)

(package! dired+)

(package! dired-hacks-utils)

(unless MINIMAL-MODE
    (package! ranger)
    (package! peep-dired)
    (package! dired-sidebar)
    (package! dired-quick-sort)
    (package! all-the-icons-dired))
