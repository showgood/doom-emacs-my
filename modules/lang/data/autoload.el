;;; lang/data/autoload.el -*- lexical-binding: t; -*-


;; print out xpath and copy to clipboard
;;;###autoload
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a
path. from http://www.emacswiki.org/emacs/NxmlMode"
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                                         ;; beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (progn
                (message "/%s" (mapconcat 'identity path "/"))
                (kill-new (format "/%s" (mapconcat 'identity path "/"))))
          (format "/%s" (mapconcat 'identity path "/")))))))


;;;###autoload
(defun xml-format ()
  "format the xml file using external tool - `xmllint`"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
))
