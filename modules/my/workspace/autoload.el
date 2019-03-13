;;; my/workspace/autoload.el -*- lexical-binding: t; -*-

;; +workspace/new does NOT take the name from user input,
 ;; this solve that issue
 ;;;###autoload
 (defun +workspace/me/new (name)
     (interactive "sEnter workspace name: ")
     (+workspace/new name)
 )

 ;;;###autoload
 (defun doom/jump-to-last-workspace ()
   "Open the previously selected workspace, if it exists."
   (interactive)
   (unless (eq 'non-existent
               (gethash doom-last-selected-workspace
                        *persp-hash* 'non-existent))
     (persp-switch doom-last-selected-workspace)))

;;;###autoload
(defun me/new-workspace-term ()
  "create a term-mode buffer which belongs to current workspace (persp-mode)
   return the newly created buffer name"
  (interactive)
  (+term/open t)
  (let ( (term-name (format "%s-term" (+workspace-current-name)))
        )
    (rename-buffer term-name t)
    (persp-add-buffer
      (current-buffer) (get-current-persp) t nil)
    term-name
  )
)

;;;###autoload
(defun me/switch-to-workspace-term ()
  "switch to the term-mode buffer for the workspace"
  (interactive)
  (let ((buf-name (format "%s-term" (+workspace-current-name))))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
      (message "buffer %s not exist!" buf-name)
      )
    )
  )

;;;###autoload
 (defun +workspace/save-name(name frame)
   (setq doom-last-selected-workspace persp-last-persp-name)
   (message (format "persp-last: %s" persp-last-persp-name))
 )
