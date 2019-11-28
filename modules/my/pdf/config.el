;;; my/pdf/config.el -*- lexical-binding: t; -*-

(when (featurep! :tools pdf)
  (use-package! pdf-tools-org
    :defer t))
