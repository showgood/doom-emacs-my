;;; my/data/config.el -*- lexical-binding: t; -*-

;; override printer to print json path in the way I want
(use-package! json-snatcher
  :defer t
  :config
  (setq jsons-path-printer 'me/jsons-print-path-as-list))
