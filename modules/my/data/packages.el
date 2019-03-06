;; -*- no-byte-compile: t; -*-
;;; my/data/packages.el

(package! json-snatcher)

;; override printer to print json path in the way I want
(setq jsons-path-printer 'me/jsons-print-path-as-list)
