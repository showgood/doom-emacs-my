;;; my/term/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun me/paste-in-term-mode()
  (interactive)
  (term-paste)
  (evil-emacs-state)
)
