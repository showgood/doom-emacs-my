;;; completion/ivy/autoload/counsel.el -*- lexical-binding: t; -*-


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
