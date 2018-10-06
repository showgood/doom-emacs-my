;;;###autoload
;; from AbcDef ==> Abc_Def
(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))
(defun underscore-string (s) (mapconcat 'downcase   (split-name s) "_"))

;;;###autoload
(defun run-command-anywhere (command infile)
  (process-file-shell-command command infile
                              (get-buffer-create "*run-cmd-anywhere*"))
  (switch-to-buffer-other-window "*run-cmd-anywhere*")
)

;;;###autoload
(defun get-local-file-name (file-name)
  (interactive)
  (if (file-remote-p default-directory)
      (tramp-file-name-localname (tramp-dissect-file-name
                                  (expand-file-name file-name)))
    (expand-file-name file-name))
)

;;;###autoload
(defun xml-reformat()
  "reformat the xml file using xmllint"
  (interactive)

  (shell-command
   (format "xmllint --format %s"
           (shell-quote-argument (buffer-file-name)))

   ;; name of output buffer
   (current-buffer)
   ;; name of the error buffer
   "*XMl reformat Error Buffer*"
))

;;;###autoload
(defun get-kill-ring()
  "get top of kill ring as plain text"
  (interactive)

  (substring-no-properties (current-kill 0))
)

;; https://superuser.com/questions/546619/clear-the-kill-ring-in-emacs
;;;###autoload
(defun clear-kill-ring ()
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect))
  )

;;;###autoload
(defun tramp/xml-reformat()
  "reformat the xml file using xmllint"
  (interactive)

  (run-command-anywhere
   (format "xmllint --format %s"
           (file-name-nondirectory
              (get-local-file-name (buffer-file-name)))) nil))

;; replace current word or selection using vim style for evil mode
;; author: showgood
;;;###autoload
(defun evil-replace-word-selection()
  "replace current word or selection using vim style for evil mode"
  (interactive)
  (if (use-region-p)
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
          (message "empty string")
          (evil-ex (concat "'<,'>s/" selection "/"))
        ))
    (evil-ex (concat "%s/" (thing-at-point 'word) "/"))))

;; insert current time
;; source: http://emacswiki.org/emacs/InsertingTodaysDate
;;;###autoload
(defun nnow ()
  (interactive)
  (insert (format-time-string "%Y-%b-%d %H:%M:%S")))

;; insert today date
;;;###autoload
(defun ddate ()
  (interactive)
  (insert (format-time-string "%Y-%b-%d")))

;; quickly dupliate a line without changing the kill-ring
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
;;;###autoload
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  (pop kill-ring)
)

;;;###autoload
(defun open-scratch()
  (interactive)
  (doom-popup-buffer (get-buffer-create  "*scratch*")))

;;;###autoload
(defun search-current-line(pattern)
  "search current line using pattern, return captured group, should
   be only 1."
  (interactive)
  (re-search-forward pattern (line-end-position))
  (match-string 1)
)

;;;###autoload
(defun extract-from-org-heading (pattern)
  (interactive)
  "go to top level heading from current position and try to extract
   using pattern"
  (let ((result nil))
    (save-excursion
      (while (org-up-heading-safe))
      (setq result (search-current-line pattern))
      (kill-new result))
      (if result
        (message "saved to kill-ring: %s" result)
        (message "no match found!"))
))

;; inspired by:
;; https://emacs.stackexchange.com/questions/3499/how-to-wrap-given-text-around-region
;; but the function from that link has issue if there is newline in the text-begin or text-end
;; and region is marked from bottom to up
;;;###autoload
(defun me/surround-region-or-word (text-begin text-end)
  "Surround current word or region with given text."
  (interactive "sStart text: \nsEnd text: ")
  (if (use-region-p)
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (progn
            (goto-char (point-min))
            (insert text-begin)
            (goto-char (point-max))
            (insert text-end)
            ))
        (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (goto-char (cdr bds))
        (insert text-end)
        (goto-char (car bds))
        (insert text-begin))
    )
)

;;;###autoload
(defun me/surround-org-src-block-from-clipboard (lang)
  "Surround clipboard content with org src block by prompting the language."
  (interactive "slanguage: ")
  (let* ((buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (insert (get-kill-ring))
      (mark-whole-buffer)
      (me/surround-region-or-word (concat "\n#+BEGIN_SRC " lang "\n") "#+END_SRC\n")
      (append-to-buffer buf (point-min) (point-max))
      )))

;;;###autoload
(defun me/create-rg-ignore-file ()
  "make a copy of rg ignore file, syntax refer to https://oremacs.com/2018/03/05/grep-exclude/"
  (interactive)
  (copy-file (format "%s/.rg-ignore" +showgood-dir) (format "%s/.ignore" default-directory))
  )

;;;###autoload
(defun me/open-module-init ()
  (interactive)
  (find-file (format "%s/config.el" +showgood-dir))
  )
