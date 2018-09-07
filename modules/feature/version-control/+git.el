;;; feature/version-control/+git.el -*- lexical-binding: t; -*-
;;;###if (not (featurep! -git))

(when (featurep! :feature evil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))


(def-package! magit
  :commands (magit-status magit-blame)
  :config
  (set! :evil-state 'magit-status-mode 'emacs)
  (after! evil
    ;; Switch to emacs state only while in `magit-blame-mode', then back when
    ;; its done (since it's a minor-mode).
    (add-hook! 'magit-blame-mode-hook
      (evil-local-mode (if magit-blame-mode -1 +1)))))

(def-package! git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)

  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details nil)
  (add-hook 'git-timemachine-mode-hook #'+vcs|init-header-line)
  (advice-add #'git-timemachine-show-revision :after #'+vcs*update-header-line)

  ;; Force evil to rehash keybindings for the current state
  (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(require 'evil-magit)

(unless MINIMAL-MODE (load! +git-extra))
