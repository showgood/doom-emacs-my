(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :commands (all-the-icons-dired-mode)))

(use-package ranger
  :config
  (setq ranger-override-dired-mode t
        ranger-preview-file t
        ranger-hide-cursor nil
        ranger-cleanup-on-disable t
        ranger-dont-show-binary t
        ranger-max-preview-size 5)
  )
