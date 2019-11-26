;;; my/mac/autoload.el -*- lexical-binding: t; -*-

;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
;;;###autoload
(defun open-in-iterm ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
)

;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
;;;###autoload
(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
;;;###autoload
(defun iterm-repeat-last-command ()
  (interactive)
  (do-applescript
   (concat
    "tell application \"iTerm2\"\n"
    "  activate\n"
    "    tell current session of current window\n"
    "      tell application \"System Events\" to keystroke (ASCII character 30)\n" ;; up arrow
    "      tell application \"System Events\" to key code 36\n" ;; return
    "    end tell\n"
    "end tell\n")))

;;;###autoload
(defun get-file-dir-or-home ()
  "If inside a file buffer, return the directory, else return home"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
    "~/"
      (file-name-directory filename))))

;;;###autoload
(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n" (get-file-dir-or-home))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )

;;;###autoload
(defun org-export-to-devonthink (arg)
  "export org file to devonthink in html format."
  (interactive "P")
  (let (content)
    (org-html-export-as-html)
    (switch-to-buffer "*Org HTML Export*")
    (setq content (buffer-string))
    (kill-buffer "*Org HTML Export*")
    (setq content (replace-regexp-in-string (regexp-quote "\"") "\\\"" content t t))
    (do-applescript
     (concat
        "tell application \"DEVONthink Pro\"\n"
        (format
           "set theRecord to create record with {name:\"%s\", type:html, content:\"%s\", url:\"%s\"} in current group\n"
           (buffer-file-name) content (buffer-file-name))
        "end tell\n"
     ))))


;;;###autoload
(defun org-devon/prev-exported-entry-id ()
  "get previous exported id if any"
  (save-excursion
    (goto-char (point-min))
       (when (re-search-forward "^#\\+DEVONTHINK:\\(.*\\)$" nil t)
           (require 'subr-x)
           (string-trim (match-string 1)))
))

;;;###autoload
(defun org-devon/delete-entry (entry-id)
  "delete entry with uuid from devonthink"
  (message "attempt to delete entry with id: %s" entry-id)
  (do-applescript
    (concat
      "tell application \"DEVONthink Pro\"\n"
      (format "set theRecord to get record with uuid \"%s\"\n" entry-id)
      "delete record theRecord\n"
      "end tell\n"
   ))
)

;;;###autoload
(defun org-devon/update-exported-id (new-id)
  "update exported entry id for the file, if not exist, insert it"
  (if (org-devon/prev-exported-entry-id)
      (progn
        (goto-char (point-min))
        (when (re-search-forward "^#\\+DEVONTHINK:\\(.*\\)$" nil t)
           (replace-match (format "#+DEVONTHINK: %s" new-id)))
         (save-buffer))

     (insert (concat "#+DEVONTHINK: " new-id))
   )
)

;;;###autoload
(defun org-devon/delete-if-exported-before ()
  (let ((prev-entry-id (org-devon/prev-exported-entry-id)))
    (when prev-entry-id
      (org-devon/delete-entry prev-entry-id)
     )
  )
)

;;;###autoload
(defun org-devon/create-record (name type content url)
  "create a new record in devonthink, return uuid of newly created entry"
  (do-applescript
    (concat
      "tell application \"DEVONthink Pro\"\n"
      (format
        "set theRecord to create record with {name:\"%s\", type:\"%s\", content:\"%s\", url:\"%s\"} in current group\n"
        name type content url)
        "set recordId to uuid of theRecord\n"
        "return recordId as string\n"
        "end tell\n"
       ))
)

;;;###autoload
(defun org-export-md-to-devonthink (arg)
  "export org file to devonthink in markdown format.
   also add some css and js to pretty the render of markdown."
  (interactive "P")
  ;; we need to check if this has already been exported before
  ;; if yes, let's delete that entry first
  (org-devon/delete-if-exported-before)

  (let ((content)
        (record-id))
    (org-md-export-as-markdown)
    (switch-to-buffer "*Org MD Export*")
    (goto-char (point-min))
    (insert "<script src=\"x-devonthink-item://2E0ED1DE-314A-4F34-9CF5-15B7A20E383F\"></script>\n")
    (insert "<script>hljs.initHighlightingOnLoad();</script>\n")
    (insert "<link type=\"text/css\" rel=\"stylesheet\" href=\"x-devonthink-item://9E6A7780-FE56-4B48-AEA1-F6620D3CAF15\" />\n")
    (insert "<link type=\"text/css\" rel=\"stylesheet\" href=\"x-devonthink-item://8CDB2C9B-F69B-4059-86AA-5AC1355671CA\" />\n")
    (insert "<link type=\"text/css\" rel=\"stylesheet\" href=\"x-devonthink-item://2F3BD48C-A7F7-4BA4-8CD3-FB9465BAD4D5\"/>\n")
    (setq content (buffer-string))
    (kill-buffer "*Org MD Export*")
    (setq content (replace-regexp-in-string (regexp-quote "\"") "\\\"" content t t))
    ;; no longer need this once we define new org link type
    ;; (setq content (replace-regexp-in-string (regexp-quote "(//") "(x-devonthink-item://" content t t))

    (setq record-id (org-devon/create-record
                     (buffer-file-name) "markdown" content (buffer-file-name)))

    (when record-id
      (org-devon/update-exported-id record-id)
    )
    (message "exported-id: %s" record-id)
))
