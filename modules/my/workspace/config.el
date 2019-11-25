;;; my/workspace/config.el -*- lexical-binding: t; -*-

(defvar doom-default-workspace-name "main"
   " name of the default layout.")

 (defvar doom-last-selected-workspace doom-default-workspace-name
   "previously selected layout.")

 (add-hook 'persp-before-switch-functions #'+workspace/save-name)
