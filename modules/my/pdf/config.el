;;; my/pdf/config.el -*- lexical-binding: t; -*-

(when (featurep! :tools pdf)
    (load! "pdf-tools-org")
    (require 'pdf-tools-org))
