;;; my/org/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun me/org-sum-rows (begin-row)
  "insert a org-table formula for sum all the rows for current column
    at the cell under cursor"
  (interactive "p")
  (cond ((or (<= begin-row 0)
             (>= begin-row (org-table-current-dline)) )
         (error "invalid row!"))
        (t
         (let ((cell (format "@%d$%d" (org-table-current-dline) (org-table-current-column))))
           (if (= begin-row 1)
               (me/org-append-formula cell
                                      (format "vsum(@2..@%d)" (- (org-table-current-dline) 1)))
             (me/org-append-formula cell
                                    (format "vsum(@%d..@%d)" begin-row (- (org-table-current-dline) 1)))
             )
           )
         )
        ))

;;;###autoload
(defun me/org-sum-cols (begin-column)
  "insert a org-table formula for sum columns begin-column -- current-column -1
   for current row at the cell under cursor"
  (interactive "p")
  (cond ((or (<= begin-column 0)
             (>= begin-column (org-table-current-column)) )
         (error "invalid column!"))
        (t
         (let (
               (cell (format "@%d$%d" (org-table-current-dline) (org-table-current-column)) )
               (sum-formula (format "vsum($%d..$%d)" begin-column (- (org-table-current-column) 1) )))
           (me/org-append-formula cell sum-formula)
           )
         )
        )
  )

;;;###autoload
(defun me/org-append-formula (cell formula)
  "build a alist (cell . formula) and append to existing formulas of the table
   also recalculate the whole table."
   (setq exist-formulas (org-table-get-stored-formulas))
   (setq new-formulas (add-to-list 'exist-formulas (cons cell formula)))
   (org-table-store-formulas new-formulas)
   ;; need to pass 'all so all rows are recalculated, not just current row
   (org-table-recalculate 'all)
)

;;;###autoload
(defun me/org-add-index-column ()
  "insert a index column to the table under cursor.
   index starting from 1. Also a hline is expected
   to exist for table for index to work properly."
  (interactive)
  (org-table-goto-column 0)
  (org-table-insert-column)
  (org-table-move-column-left)
  (me/org-append-formula "$1" "@#-1")
)

;;;###autoload
(defun me/create-org-table-from-clipboard ()
  "create a table in org mode with content from clipboard"
  (interactive)
  (let* ((buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (insert (get-kill-ring))
      (mark-whole-buffer)
      (org-table-create-or-convert-from-region nil)
      (org-table-insert-hline)
      (goto-char (point-min))
      (open-line 1)
      (insert "#+tblname:")
      (append-to-buffer buf (point-min) (point-max))
      )))

;;;###autoload
(defun ha/external-capture-to-org ()
  "Calls `org-capture-string' on the contents of the Apple clipboard."
  (interactive)
  (org-capture-string (ha/org-clipboard) "C")
  (ignore-errors
    (delete-frame)))

;;;###autoload
(defun ha/org-clipboard ()
  "Return the contents of the clipboard in org-mode format."
  (destructuring-bind (type contents) (ha/get-clipboard)
    (with-temp-buffer
      (insert contents)
      (if (eq :html type)
          (shell-command-on-region (point-min) (point-max) "pandoc -f html -t org" t t)
        (shell-command-on-region (point-min) (point-max) "pandoc -f markdown -t org" t t))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun ha/org-yank-clipboard ()
  "Yanks (pastes) the contents of the Apple Mac clipboard in an
org-mode-compatible format."
  (interactive)
  (insert (ha/org-clipboard)))

;;;###autoload
(defun ha/get-mac-clipboard ()
  "Returns a list where the first entry is the content type,
either :html or :text, and the second is the clipboard contents."
  (destructuring-bind (exit-code contents)
      (shell-command-with-exit-code "osascript" "-e" "the clipboard as \"HTML\"")
    (if (= 0 exit-code)
        (list :html (ha/convert-applescript-to-html contents))
      (list :text (shell-command-to-string "osascript -e 'the clipboard'")))))


;;;###autoload
(defun shell-command-with-exit-code (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

;;;###autoload
(defun ha/get-clipboard ()
  "Returns a list where the first entry is the content type,
either :html or :text, and the second is the clipboard contents."
  (if (eq system-type 'darwin)
      (ha/get-mac-clipboard)
    (ha/get-linux-clipboard)))


;;;###autoload
(defun ha/get-linux-clipboard ()
  "Return the clipbaard for a Unix-based system. See `ha/get-clipboard'."
  (destructuring-bind (exit-code contents)
      (shell-command-with-exit-code "xclip" "-o" "-t" "text/html")
    (if (= 0 exit-code)
        (list :html contents)
      (list :text (shell-command-to-string "xclip -o")))))


;;;###autoload
(defun ha/org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org SRC block with a language based on the current mode
and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (ha/org-capture-fileref-snippet f "SRC" org-src-mode func-name))))

;;;###autoload
(defun ha/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (ha/org-capture-fileref-snippet f "EXAMPLE" "" nil)))

;;;###autoload
(defun ha/org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "From [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
   %s

   #+BEGIN_%s %s
%s
   #+END_%s" initial-txt type headers code-snippet type)))

;;;###autoload
(defun ha/code-to-clock (&optional start end)
  "Send the currently selected code to the currently clocked-in org-mode task."
  (interactive)
  (org-capture nil "F"))

;;;###autoload
(defun ha/code-comment-to-clock (&optional start end)
  "Send the currently selected code (with comments) to the
currently clocked-in org-mode task."
  (interactive)
  (org-capture nil "f"))


;;;###autoload
(defun ha/command-html-to-clipboard (html-file)
  "Return a command (suitable for `shell-command') to convert the
contents of HTML-FILE to the operating system's clipboard."
  (if (eq system-type 'darwin)
      (concat "hex=`hexdump -ve '1/1 \"%.2x\"' < "
              html-file
              "` \n"
              "osascript -e \"set the clipboard to «data HTML${hex}»\"")
    ;; TODO Add a version to convert HTML to Linux clipboard
    (concat "xclip -t text/html " html-file)))

;;;###autoload
(defun ha/org-html-with-header-to-clipboard ()
  "Converts region or subtree (with the section header) of the
current org file into HTML and copies the contents as HTML into
the operating system's clipboard."
  (interactive)
  (let ((html-file (org-html-export-to-html nil t t)))
    (shell-command (ha/command-html-to-clipboard html-file))))

;;;###autoload
(defun ha/org-html-to-clipboard ()
   "Converts region or subtree of the current org file into HTML
and copies the contents as HTML into the operating system's
clipboard."
   (interactive)
   (let ((html-file (org-html-export-to-html nil t t t)))
     (shell-command (ha/command-html-to-clipboard html-file))))

;;;###autoload
(defun ha/command-file-to-clipboard (md-file)
  "Return a command (suitable for `shell-command') to convert the
contents of MD-FILE to the operating system's clipboard."
  (if (eq system-type 'darwin)
      (concat "pbcopy < " md-file)
    (concat "xclip " md-file)))

;;;###autoload
(defun ha/org-to-md-clipboard ()
  "Converts region or subtree of the current org file into
Markdown and copies the contents into the operating system's
clipboard."
  (interactive)
  (let ((text-file (org-md-export-to-markdown nil t t)))
    (shell-command (ha/command-file-to-clipboard text-file))))

;;;###autoload
(defun ha/convert-applescript-to-html (packed-contents)
  "Applescript's clipboard returns the contents in a packed array.
Convert and return this encoding into a UTF-8 string."
  (cl-flet ((hex-pack-bytes (tuple) (string-to-number (apply 'string tuple) 16)))
    (let* ((data (-> packed-contents
                     (substring 10 -2) ; strips off the =«data RTF= and =»\= bits
                     (string-to-list)))
           (byte-seq (->> data
                          (-partition 2)  ; group each two hex characters into tuple
                          (mapcar #'hex-pack-bytes))))

      (decode-coding-string
       (mapconcat #'byte-to-string byte-seq "") 'utf-8))))


;; https://emacs.stackexchange.com/questions/24676/html-to-orgmode-via-pandoc-get-rid-of-all-begin-html-blocks
;;;###autoload
(defun me/clean-up-pandoc-org-conversion ()
  (interactive)
  (replace-regexp (rx (optional "\n")
                    "#+BEGIN_HTML"
                    (minimal-match (1+ anything))
                    "#+END_HTML"
                    (optional "\n"))
                "")
)
