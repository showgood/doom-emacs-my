;;; my/ivy/autoload.el -*- lexical-binding: t; -*-

;; https://protesilaos.com/dotemacs/
;;;###autoload
(defun prot/counsel-fzf-rg-files (&optional input dir)
  "Run `fzf' in tandem with `ripgrep' to find files in the
present directory.  If invoked from inside a version-controlled
repository, then the corresponding root is used instead."
  (interactive)
  (let* ((process-environment
          (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden")
                process-environment))
          (vc (vc-root-dir)))
    (if dir
        (counsel-fzf input dir)
      (if (eq vc nil)
          (counsel-fzf input default-directory)
        (counsel-fzf input vc)))))

;;;###autoload
(defun prot/counsel-fzf-dir (arg)
  "Specify root directory for `counsel-fzf'."
  (prot/counsel-fzf-rg-files ivy-text
                              (read-directory-name
                              (concat (car (split-string counsel-fzf-cmd))
                                      " in directory: "))))

;;;###autoload
(defun prot/counsel-rg-dir (arg)
  "Specify root directory for `counsel-rg'."
  (let ((current-prefix-arg '(4)))
    (counsel-rg ivy-text nil "")))

;;;###autoload
;; TODO generalise for all relevant file/buffer counsel-*?
(defun prot/counsel-fzf-ace-window (arg)
  "Use `ace-window' on `prot/counsel-fzf-rg-files' candidate."
  (ace-window t)
  (let ((default-directory (if (eq (vc-root-dir) nil)
                                counsel--fzf-dir
                              (vc-root-dir))))
    (if (> (length (aw-window-list)) 1)
        (progn
          (find-file arg))
      (find-file-other-window arg))
    (balance-windows)))

;;;###autoload
(defun me/get-key (x separator)
  (kill-new (string-trim (car (split-string x separator t))))
)

;;;###autoload
(defun me/get-value (x separator)
  (kill-new (string-trim (cadr (split-string x separator t))))
)

;;;###autoload
(defun me/ivy-transform-with-separator (s)
  (replace-regexp-in-string me/ivy-separator "   " s)
)

;;;###autoload
(defun me/ivy-from-file (file)
  "read lines from file and display two column list by using me/ivy-separator"
  (interactive)
  (ivy-read "options: " (me/read-lines file)
            :action '(1
                      ("o" (lambda (x) (me/get-key x me/ivy-separator)) "get key")
                      ("j" (lambda (x) (me/get-value x me/ivy-separator)) "get value"))))

;; for eg, below is an example for using this me/ivy-from-file
;; (defvar me/ivy-separator "\\$\\$\\$")

;; (defun me/test-ivy ()
;;   (interactive)
;;   (ivy-set-display-transformer 'me/test-ivy 'me/ivy-transform-with-separator)
;;   (me/ivy-from-file "~/.doom.d/test.txt"))

;;;###autoload
(defun me/ivy-menu (file)
  "nested ivy case. the file would act as menu where key is menu entry name and value is the target file.
  Then upon select one entry from menu, it will call `me/ivy-from-file' to read that file and present all choices"
  (interactive)
  (ivy-read "options: " (me/read-lines file)
            :action '(1
                      ("o" (lambda (x) (me/ivy-from-file (me/get-value x me/ivy-separator)) "open file")))))
