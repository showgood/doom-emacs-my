;;; feature/version-control/+git.el -*- lexical-binding: t; -*-
;;;###if (not (featurep! -git))

(when (featurep! :feature evil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

(unless (equal doom-mode "minimal")
  (load! +git-extra)
)
