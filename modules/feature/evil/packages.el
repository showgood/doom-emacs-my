;; -*- no-byte-compile: t; -*-
;;; feature/evil/packages.el

(package! evil)
(package! evil-commentary)
(package! evil-escape)
(package! evil-matchit)
(package! evil-snipe)
(package! evil-surround)
(package! evil-visualstar)

;; (package! evil-args)
;; (package! evil-easymotion)
;; (package! evil-embrace)
;; (package! evil-indent-plus)

(package! evil-replace-with-register)

(unless (equal doom-mode "minimal")
    (package! evil-vimish-fold)
    (package! evil-mc)
    (package! evil-multiedit)
    (package! evil-numbers)
    (package! evil-textobj-anyblock)
    (package! evil-exchange)
    (package! evil-ediff)
    (package! evil-visual-mark-mode)
)
