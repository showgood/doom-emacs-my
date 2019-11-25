;;; my/term/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun me/paste-in-term-mode()
  (interactive)
  (term-paste)
  (evil-emacs-state)
)

;;;###autoload
(defun me/back-to-term-normal ()
  (interactive)
  (term-line-mode)
  (evil-normal-state)
)

;;;###autoload
(defun evil-collection-term-char-mode-insert ()
  "Switch to `term-char-mode' and enter insert state."
  (interactive)
  (term-char-mode)
  (evil-insert-state))

;;;###autoload
(defun setup-my-term-mode()
  (setq-local global-hl-line-mode nil)
  ;; (setq-local beacon-mode nil)
  (setq term-buffer-maximum-size 0)
  ;; (key-chord-define term-raw-map ",," 'me/back-to-term-normal)
  ;; (key-chord-define term-mode-map ",," 'me/back-to-term-normal)
)
