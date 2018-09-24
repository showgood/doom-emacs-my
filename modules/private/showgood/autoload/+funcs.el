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

;;;###autoload
(defun Open ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-06-12"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "cygwin")
    (w32-shell-execute "explore" default-directory ))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

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
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
))

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

;; http://blog.binchen.org/posts/complete-line-with-ivy-mode.html
;;;###autoload
(defun counsel-escape (keyword)
  (setq keyword (replace-regexp-in-string "\\$" "\\\\\$" keyword))
  (replace-regexp-in-string "\"" "\\\\\"" keyword))

;;;###autoload
(defun counsel-replace-current-line (leading-spaces content)
  (beginning-of-line)
  (kill-line)
  (insert (concat leading-spaces content))
  (end-of-line))

;;;###autoload
(defun counsel-git-grep-complete-line ()
  (interactive)
  (let* (cmd
        (cur-line (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position)))
        (default-directory (locate-dominating-file
                            default-directory ".git"))
        keyword
        (leading-spaces "")
        collection)
    (setq keyword (counsel-escape (if (region-active-p)
                                      (buffer-substring-no-properties (region-beginning)
                                                                      (region-end))
                                    (replace-regexp-in-string "^[ \t]*" "" cur-line))))

    ;; grep lines without leading/trailing spaces
    (setq cmd (format "git --no-pager grep -I -h --no-color -i -e \"^[ \\t]*%s\" | sed s\"\/^[ \\t]*\/\/\" | sed s\"\/[ \\t]*$\/\/\" | sort | uniq" keyword))
    (when (setq collection (split-string (shell-command-to-string cmd) "\n" t))
      (if (string-match "^\\([ \t]*\\)" cur-line)
          (setq leading-spaces (match-string 1 cur-line)))
      (cond
       ((= 1 (length collection))
        (counsel-replace-current-line leading-spaces (car collection)))
       ((> (length collection) 1)
        (ivy-read "lines:"
                  collection
                  :action (lambda (l)
                            (counsel-replace-current-line leading-spaces l))))))
    ))
;; (global-set-key (kbd "C-x C-l") 'counsel-git-grep-complete-line)

;; http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
;;;###autoload
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
        (kill-new val)
        (message "%s => kill-ring" val))))

;;;###autoload
(defun counsel-yank-zsh-history ()
    "Yank the zsh history"
    (interactive)
    (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
    (nreverse
    (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
    (buffer-string))
    "\n"
    t)))

    (setq collection (mapcar (lambda (it) (replace-regexp-in-string ".*;" "" it)) collection)) ;; for zsh

    (when (and collection (> (length collection) 0)
    (setq val (if (= 1 (length collection)) (car collection)
    (ivy-read (format "Zsh history:") collection))))
    ;; (setq val (replace-regexp-in-string "^:[^;]*;" "" val))
    ;; (setq val (replace-regexp-in-string ".*;" "" val))
    (kill-new val)
(message "%s => kill-ring" val))))

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
(defun me/create-clang-format ()
  "make a copy of clang-format file"
  (interactive)
  (copy-file (format "%s/my.clang-format" +showgood-dir) (format "%s/.clang-format" default-directory))
  )

;;;###autoload
(defun me/create-rg-ignore-file ()
  "make a copy of rg ignore file, syntax refer to https://oremacs.com/2018/03/05/grep-exclude/"
  (interactive)
  (copy-file (format "%s/.rg-ignore" +showgood-dir) (format "%s/.ignore" default-directory))
  )

;;;###autoload
(defun me/switch-to-project-term ()
  (interactive)
  (let ((buf-name (format "%s-term" (+workspace-current-name))))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
      (message "buffer %s not exist!" buf-name)
      )
    )
  )

;;;###autoload
(defun me/open-module-init ()
  (interactive)
  (find-file (format "%s/config.el" +showgood-dir))
  )
