;;; tools/dired/config.el -*- lexical-binding: t; -*-

(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

 ;;http://irreal.org/blog/?p=3341
 ;; display file details for dired
 ;; this needs to happen before loading dired+
(setq diredp-hide-details-initially-flag nil)

(use-package dired+
  :config
  (diredp-make-find-file-keys-reuse-dirs)
  )

(setq ;; Always copy/delete recursively
 dired-recursive-copies  'always
 dired-recursive-deletes 'always

 ;; Auto refresh dired, but be quiet about it
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 ;; when using find-dired, also list the result with size etc
 find-ls-option '("-print0 | xargs -0 ls -alh" . "")

 ;; delete file permanently, do not move to trash bin
 delete-by-moving-to-trash nil

 ;; https://www.reddit.com/r/emacs/comments/1493oa/emacsmovies_season_2_dired/
 ;; make df output in dired buffers easier to read
 dired-free-space-args "-pm"

 ;; try suggesting dired targets
 dired-dwim-target t

 ;; files
 image-dired-dir (concat doom-cache-dir "image-dired/")
 image-dired-db-file (concat image-dired-dir "image-dired/db.el")
 image-dired-gallery-dir (concat image-dired-dir "gallery/")
 image-dired-temp-image-file (concat image-dired-dir "temp-image")
 image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))

;; understand .zip the way it does tarballs, letting the z key decompress it:
;; handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(defun +dired|sort-directories-first ()
  "List directories first in dired buffers."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook #'+dired|sort-directories-first)

;; Automatically create missing directories when creating new files
(defun +dired|create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(push #'+dired|create-non-existent-directory find-file-not-found-functions)

(after! evil
  (add-transient-hook! 'dired-mode-hook
    (map! :map dired-mode-map
          :n "c" #'find-file
          :n "d" #'dired-do-delete
          :n "r" #'dired-do-rename)))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :commands (all-the-icons-dired-mode)))

;; (setq dired-sidebar-subtree-line-prefix " .")

(when IS-MAC
    (if (display-graphic-p)
        (setq dired-sidebar-theme 'icons)
      (setq dired-sidebar-theme 'nerd)))

;;
;; Packages
;;

(def-package! dired-k
  :after dired
  :config
  (setq dired-k-style 'git)

  (defun +dired*dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired*dired-k-highlight)

  (add-hook 'dired-initial-position-hook #'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(def-package! stripe-buffer
  :commands stripe-buffer-mode
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

;; https://oremacs.com/2015/01/21/dired-shortcuts/
(general-define-key
:states '(normal visual insert emacs)
:keymaps 'dired-mode-map
:prefix ","
:non-normal-prefix "M-SPC"
"f" '(find-name-dired :which-key "find name dired")

;; this will go to parent folder using existing dired buffer
"a" '(lambda ()
      (interactive)
      (find-alternate-file "..") :which-key "go to parent folder")
)

(general-define-key
:states '(normal visual insert emacs)
:keymaps 'dired-mode-map
 "C-h" '(evil-window-left :which-key "left window")
 "C-j" '(evil-window-down :which-key "down window")
 "C-k" '(evil-window-up :which-key "up window")
 "C-l" '(evil-window-right :which-key "right window")
)

;; https://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
(when IS-MAC
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil)
)
