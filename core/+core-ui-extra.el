(def-package! fringe-helper
  :commands (fringe-helper-define fringe-helper-convert)
  :init
  (unless (fboundp 'define-fringe-bitmap)
    ;; doesn't exist in terminal Emacs; define it to prevent errors
    (defun define-fringe-bitmap (&rest _))))


(def-package! highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode))


;; For a distractions-free-like UI, that dynamically resizes margets and can
;; center a buffer.
(def-package! visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default
   visual-fill-column-center-text t
   visual-fill-column-width
   ;; take Emacs 26 line numbers into account
   (+ (if (boundp 'display-line-numbers) 6 0)
      fill-column)))
