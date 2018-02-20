;;; tools/term/config.el -*- lexical-binding: t; -*-

(def-package! multi-term
  :commands (multi-term multi-term-next multi-term-prev)
  :config
  (setq multi-term-program (getenv "SHELL")
        multi-term-switch-after-close 'PREVIOUS))

(setq multi-term-dedicated-select-after-open-p t)

(defun setup-my-term-mode()
  (setq-local global-hl-line-mode nil)
  (setq-local beacon-mode nil)
  (setq term-buffer-maximum-size 0)
)

(add-hook 'term-mode-hook #'setup-my-term-mode)

(general-define-key
 :states '(normal)
 :keymaps 'term-raw-map
 "p" '(me/paste-in-term-mode :which-key "paste")
 "i" '(evil-emacs-state :which-key "insert")
 "a" '(evil-emacs-state :which-key "insert")
 "C-y" '(me/paste-in-term-mode :which-key "paste")
)

(general-define-key
 :states '(insert emacs)
 :keymaps 'term-raw-map
 "C-;" '(evil-normal-state :which-key "escape")
 "C-y" '(me/paste-in-term-mode :which-key "paste")
)
