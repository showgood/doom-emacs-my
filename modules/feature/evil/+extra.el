(def-package! evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match))


(def-package! evil-mc
  :commands (evil-mc-make-cursor-here evil-mc-make-all-cursors
             evil-mc-undo-all-cursors evil-mc-pause-cursors
             evil-mc-resume-cursors evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)

  ;; Add custom commands to whitelisted commands
  (dolist (fn '(doom/deflate-space-maybe doom/inflate-space-maybe
                doom/backward-to-bol-or-indent doom/forward-to-last-non-comment-or-eol
                doom/backward-kill-to-bol-and-indent doom/newline-and-indent))
    (push (cons fn '((:default . evil-mc-execute-default-call)))
          evil-mc-custom-known-commands))

  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes)

  (defun +evil|escape-multiple-cursors ()
    "Clear evil-mc cursors and restore state."
    (when (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)
      (evil-mc-resume-cursors)
      t))
  (add-hook '+evil-esc-hook #'+evil|escape-multiple-cursors))


(use-package evil-numbers
:commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
)

;;
;; Multiple cursors compatibility (for the plugins that use it)
;;

;; mc doesn't play well with evil, this attempts to assuage some of its problems
;; so that any plugins that depend on multiple-cursors (which I have no control
;; over) can still use it in relative safety.
(after! multiple-cursors-core
  (map! :map mc/keymap :ne "<escape>" #'mc/keyboard-quit)

  (defvar +evil--mc-compat-evil-prev-state nil)
  (defvar +evil--mc-compat-mark-was-active nil)

  (defsubst +evil--visual-or-normal-p ()
    "True if evil mode is enabled, and we are in normal or visual mode."
    (and (bound-and-true-p evil-mode)
         (not (memq evil-state '(insert emacs)))))

  (defun +evil|mc-compat-switch-to-emacs-state ()
    (when (+evil--visual-or-normal-p)
      (setq +evil--mc-compat-evil-prev-state evil-state)
      (when (region-active-p)
        (setq +evil--mc-compat-mark-was-active t))
      (let ((mark-before (mark))
            (point-before (point)))
        (evil-emacs-state 1)
        (when (or +evil--mc-compat-mark-was-active (region-active-p))
          (goto-char point-before)
          (set-mark mark-before)))))

  (defun +evil|mc-compat-back-to-previous-state ()
    (when +evil--mc-compat-evil-prev-state
      (unwind-protect
          (case +evil--mc-compat-evil-prev-state
            ((normal visual) (evil-force-normal-state))
            (t (message "Don't know how to handle previous state: %S"
                        +evil--mc-compat-evil-prev-state)))
        (setq +evil--mc-compat-evil-prev-state nil)
        (setq +evil--mc-compat-mark-was-active nil))))

  (add-hook 'multiple-cursors-mode-enabled-hook '+evil|mc-compat-switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook '+evil|mc-compat-back-to-previous-state)
)

  (defun +evil|mc-evil-compat-rect-switch-state ()
    (if rectangular-region-mode
        (+evil|mc-compat-switch-to-emacs-state)
      (setq +evil--mc-compat-evil-prev-state nil)))

  ;; When running edit-lines, point will return (position + 1) as a
  ;; result of how evil deals with regions
  (defadvice mc/edit-lines (before change-point-by-1 activate)
    (when (+evil--visual-or-normal-p)
      (if (> (point) (mark))
          (goto-char (1- (point)))
        (push-mark (1- (mark))))))

  (add-hook 'rectangular-region-mode-hook '+evil|mc-evil-compat-rect-switch-state)

  (defvar mc--default-cmds-to-run-once nil)

(def-package! evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook '+evil-esc-hook #'+evil|escape-exchange))


(def-package! evil-vimish-fold
  :commands evil-vimish-fold-mode
  :init
  (setq vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  (add-hook 'doom-post-init-hook #'evil-vimish-fold-mode t))

(def-package! evil-textobj-anyblock
  :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block))
