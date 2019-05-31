;;; my/mac/emacs-dayone2.el -*- lexical-binding: t; -*-

;; adopted from
;; https://github.com/ganmacs/emacs-dayone/blob/master/emacs-dayone.el

;; author: showgood
;; This package requires following:
;; - ox-gfm for exporting org mode content to markdown
;; - dayone2 cli tool
;; dayone2 cli tools can be installed via:
;; sudo bash /Applications/Day\ One.app/Contents/Resources/install_cli.sh
;; for more information on dayone2 cli:
;; http://help.dayoneapp.com/tips-and-tutorials/command-line-interface-cli

(defvar emacs-dayone-cmd "dayone2 new")
(defvar emacs-dayone-succes-reg "^Created new entry with uuid:")

;;;###autoload
(defun emacs-dayone2/save (entry-time tags &optional b e)
  "Save current region to DayOne 2"
  (interactive "r")
  (let* ((start (if mark-active b (point-min)))
         (end   (if mark-active e (point-max)))
         (contents (buffer-substring start end))
         (tmp-file (make-temp-file "emacs-dayone")))
    (with-temp-file tmp-file (insert contents))
    (message (number-to-string  start))
    (emacs-dayone2/save-from-file tmp-file entry-time tags)))

;;;###autoload
(defun emacs-dayone/export-to-md ()
  (org-gfm-export-to-markdown)
  (concat (buffer-file-name) ".md")
)

;;;###autoload
(defun emacs-dayone2/save-as-md ()
  "Export current region as markdown and
   save to DayOne"
  (interactive)
  (emacs-dayone2/save-from-file (emacs-dayone/export-to-md))
)

;;;###autoload
(defun emacs-dayone2/save-from-file (tmp-file entry-time tags)
  "Save the content from tmp-file to DayOne"
  (interactive "r")
   (if (string-match emacs-dayone-succes-reg
          (shell-command-to-string
             (format "%s < %s --date='%s' --tags %s" emacs-dayone-cmd tmp-file entry-time tags)))
        (message "Success: contents saved")
      (message "Failed: can't saved"))
    (delete-file tmp-file))


;;;###autoload
(defun emacs-dayone2/org-journal-export-to-dayone2 ()
  "take an org journal file and export all of the second level headings
   to DayOne 2 for Mac."
  (interactive)
      (org-map-entries
        (lambda ()
          (let* ((title (nth 4 (org-heading-components)))
                 (tags (nth 5 (org-heading-components)))
                (time (substring title 0 5))
                (date (buffer-name))
                (entry-time
                 (format "%s-%s-%s %s:00"
                         (substring date 0 4)
                         (substring date 4 6)
                         (substring date 6 8)
                         time))
               (export-time (org-entry-get (point) "EXPORT_TIME"))
               (level (car (org-heading-components))))
            (when (eq level 2)
              (unless export-time
                (org-set-property "EXPORT_TIME"
                                  (format "[%s]" entry-time))
                (message "process %s, entry-time: %s" title entry-time)
                (org-gfm-export-as-markdown nil t)
                (switch-to-buffer "*Org GFM Export*")
                (goto-char (point-min))
                (while (search-forward-regexp "^#" nil t)
                  (replace-match "##"))
                (goto-char (point-min))
                (insert (format "# %s\n\n" (substring title 6)))
                (if tags
                    (progn
                        (setq tags (split-string tags ":" t))
                        (add-to-list 'tags "org_journal")
                    )
                  (setq tags '("org_journal"))
                )

                (setq tags (mapconcat 'identity tags " "))
                (emacs-dayone2/save entry-time tags)
                (kill-buffer "*Org GFM Export*"))))
)))

(provide 'emacs-dayone2)
