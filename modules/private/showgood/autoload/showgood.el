(defmacro +hlissner-def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "showgood/find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir)
             projectile-project-name
             projectile-require-project-root
             projectile-cached-buffer-file-name
             projectile-cached-project-root)
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "showgood/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload 'showgood/find-in-templates "private/showgood/autoload/showgood"  nil t)
;;;###autoload (autoload 'showgood/browse-templates "private/showgood/autoload/showgood"  nil t)
(+hlissner-def-finder! templates +file-templates-dir)

;;;###autoload (autoload 'showgood/find-in-snippets "private/showgood/autoload/showgood"  nil t)
;;;###autoload (autoload 'showgood/browse-snippets "private/showgood/autoload/showgood"  nil t)
(+hlissner-def-finder! snippets +showgood-snippets-dir)

;;;###autoload (autoload 'showgood/find-in-dotfiles "private/showgood/autoload/showgood"  nil t)
;;;###autoload (autoload 'showgood/browse-dotfiles "private/showgood/autoload/showgood"  nil t)
(+hlissner-def-finder! dotfiles (expand-file-name "dotfiles" "~"))

;;;###autoload (autoload 'showgood/find-in-emacsd "private/showgood/autoload/showgood"  nil t)
;;;###autoload (autoload 'showgood/browse-emacsd "private/showgood/autoload/showgood"  nil t)
(+hlissner-def-finder! emacsd +showgood-dir)

;;;###autoload (autoload 'showgood/find-in-notes "private/showgood/autoload/showgood"  nil t)
;;;###autoload (autoload 'showgood/browse-notes "private/showgood/autoload/showgood"  nil t)
(+hlissner-def-finder! notes +org-dir)

;;;###autoload (autoload 'showgood/find-in-docs "private/showgood/autoload/showgood"  nil t)
;;;###autoload (autoload 'showgood/browse-docs "private/showgood/autoload/showgood"  nil t)
(+hlissner-def-finder! docs (expand-file-name "docs" "~"))
