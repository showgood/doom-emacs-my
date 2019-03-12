;;; my/cquery/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cquery//enable ()
  (condition-case nil
      (lsp)
    (user-error nil)))
