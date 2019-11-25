;;; my/python/config.el -*- lexical-binding: t; -*-

(add-hook 'python-mode-hook
      (lambda ()
        (setq tab-width 4)
        (setq python-indent-offset 4)))
