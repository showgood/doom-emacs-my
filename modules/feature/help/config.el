;;; feature/help/config.el -*- lexical-binding: t; -*-


(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path "~/tldr")
)

(def-package! dash-at-point
  :commands dash-at-point
)
