;;; my/term/config.el -*- lexical-binding: t; -*-

(setq multi-term-dedicated-select-after-open-p t)

;; NOTE: need to disable evil-collection for term
;; otherwise my keybindings won't work
(push 'term +evil-collection-disabled-list)

(after! term
    (add-hook 'term-mode-hook #'setup-my-term-mode)
    (general-define-key
    :states 'normal
    :keymaps '(term-raw-map term-mode-map)
    "p" '(me/paste-in-term-mode :which-key "paste")
    "i" '(evil-collection-term-char-mode-insert :which-key "insert")
    "a" '(evil-insert-state :which-key "insert")
    "C-y" '(me/paste-in-term-mode :which-key "paste")
    "C-z" '(comint-clear-buffer :which-key "clear buffer")
    "C-h" '(evil-window-left :which-key "left window")
    "C-j" '(evil-window-down :which-key "down window")
    "C-k" '(evil-window-up :which-key "up window")
    "C-l" '(evil-window-right :which-key "right window")
    )

    (general-define-key
    :states '(insert emacs)
    :keymaps '(term-raw-map term-mode-map)
    "C-;" '(me/back-to-term-normal :which-key "escape")
    "C-y" '(term-paste :which-key "paste")
    "C-k" '(term-send-up :which-key "up")
    "C-j" '(term-send-down :which-key "<down>")
    "C-z" '(comint-clear-buffer :which-key "clear buffer")
    ;; this also works by simulating the key as up/down
    ;; "C-k" (general-simulate-key "<up>")
    ;; "C-j" (general-simulate-key "<down>")
    )
)

(def-package! isend-mode
  :defer t)
