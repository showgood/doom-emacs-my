;;; feature/evil/config.el -*- lexical-binding: t; -*-

;; I'm a vimmer at heart. Its modal philosophy suits me better, and this module
;; strives to make Emacs a much better vim than vim was.

(def-setting! :evil-state (modes state)
  "Set the initialize STATE of MODE using `evil-set-initial-state'."
  (let ((unquoted-modes (doom-unquote modes)))
    (if (listp unquoted-modes)
        `(progn
           ,@(cl-loop for mode in unquoted-modes
                      collect `(evil-set-initial-state ',mode ,state)))
      `(evil-set-initial-state ,modes ,state))))


;;
;; evil-mode
;;

(autoload 'goto-last-change "goto-chg")
(autoload 'goto-last-change-reverse "goto-chg")

(def-package! evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil)

  :config
  (add-hook 'doom-init-hook #'evil-mode)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (set! :popup
    '("*evil-registers*" :size 0.3)
    '("*Command Line*" :size 8))

  ;; Don't interfere with localleader key
  (define-key evil-motion-state-map "\\" nil)

  ;; Set cursor colors later, once theme is loaded
  (defun +evil*init-cursors (&rest _)
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor  `(,(face-foreground 'warning) box)
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'+evil*init-cursors)

  ;; default modes
  (dolist (mode '(tabulated-list-mode view-mode comint-mode term-mode calendar-mode Man-mode grep-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(help-mode debugger-mode))
    (evil-set-initial-state mode 'normal))

  ;; make `try-expand-dabbrev' from `hippie-expand' work in minibuffer
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
  (defun minibuffer-inactive-mode-hook-setup ()
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook #'minibuffer-inactive-mode-hook-setup)


  ;; --- keybind fixes ----------------------
  (map! (:after wgrep
          ;; a wrapper that invokes `wgrep-mark-deletion' across lines
          ;; you use `evil-delete' on.
          :map wgrep-mode-map [remap evil-delete] #'+evil-delete)

        ;; replace native folding commands
        [remap evil-toggle-fold]   #'+evil:fold-toggle
        [remap evil-close-fold]    #'+evil:fold-close
        [remap evil-open-fold]     #'+evil:fold-open
        [remap evil-open-fold-rec] #'+evil:fold-open
        [remap evil-close-folds]   #'+evil:fold-close-all
        [remap evil-open-folds]    #'+evil:fold-open-all)


  ;; --- evil hacks -------------------------
  (defvar +evil-esc-hook '(t)
    "A hook run after ESC is pressed in normal mode (invoked by
`evil-force-normal-state'). If any hook returns non-nil, all hooks after it are
ignored.")

  (defun +evil*attach-escape-hook ()
    "Run the `+evil-esc-hook'."
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (abort-recursive-edit))
          ((evil-ex-hl-active-p 'evil-ex-search)
           ;; disable ex search buffer highlights.
           (evil-ex-nohighlight))
          (t
           ;; Run all escape hooks. If any returns non-nil, then stop there.
           (run-hook-with-args-until-success '+evil-esc-hook))))
  (advice-add #'evil-force-normal-state :after #'+evil*attach-escape-hook)

  (defun +evil*restore-normal-state-on-windmove (orig-fn &rest args)
    "If in anything but normal or motion mode when moving to another window,
restore normal mode. This prevents insert state from bleeding into other modes
across windows."
    (unless (memq evil-state '(normal motion emacs))
      (evil-normal-state +1))
    (apply orig-fn args))
  (advice-add #'windmove-do-window-select :around #'+evil*restore-normal-state-on-windmove)

  (defun +evil*static-reindent (orig-fn &rest args)
    "Don't move cursor on indent."
    (save-excursion (apply orig-fn args)))
  (advice-add #'evil-indent :around #'+evil*static-reindent)

  ;; monkey patch `evil-ex-replace-special-filenames' to add more ex
  ;; substitution flags to evil-mode
  (advice-add #'evil-ex-replace-special-filenames :override #'doom-resolve-vim-path)

  ;; These arg types will highlight matches in the current buffer
  (evil-ex-define-argument-type buffer-match :runner +evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner +evil-ex-global-match)

  ;; By default :g[lobal] doesn't highlight matches in the current buffer. I've
  ;; got to write my own argument type and interactive code to get it to do so.
  (evil-ex-define-argument-type global-delim-match :runner +evil-ex-global-delim-match)

  (dolist (sym '(evil-ex-global evil-ex-global-inverted))
    (evil-set-command-property sym :ex-arg 'global-delim-match))

  ;; Other commands can make use of this
  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match (list (when (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<//g>"
    :ex-arg global-match (list (when (evil-ex-p) evil-ex-argument)))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-set-command-properties
   '+evil:align :move-point t :ex-arg 'buffer-match :ex-bang t :evil-mc t :keep-visual t :suppress-operator t)
  (evil-set-command-properties
   '+evil:mc :move-point nil :ex-arg 'global-match :ex-bang t :evil-mc t)

  ;; Move to new split -- setting `evil-split-window-below' &
  ;; `evil-vsplit-window-right' to non-nil mimics this, but that doesn't update
  ;; window history. That means when you delete a new split, Emacs leaves you on
  ;; the 2nd to last window on the history stack, which is jarring.
  (defun +evil*window-follow (&rest _)  (evil-window-down 1))
  (defun +evil*window-vfollow (&rest _) (evil-window-right 1))
  (advice-add #'evil-window-split  :after #'+evil*window-follow)
  (advice-add #'evil-window-vsplit :after #'+evil*window-vfollow))


;;
;; Plugins
;;

(def-package! evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(def-package! evil-escape
  :commands evil-escape-mode
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-key-sequence "jf"
        evil-escape-delay 0.25)
  (add-hook 'doom-post-init-hook #'evil-escape-mode)
  :config
  ;; no `evil-escape' in minibuffer
  (push #'minibufferp evil-escape-inhibit-functions)
  (map! :irvo "C-g" #'evil-escape))


(def-package! evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config (global-evil-matchit-mode 1)
  :init
  (map! [remap evil-jump-item] #'evilmi-jump-items
        :textobj "%" #'evilmi-text-object #'evilmi-text-object)
  :config
  (defun +evil|simple-matchit ()
    "A hook to force evil-matchit to favor simple bracket jumping. Helpful when
the new algorithm is confusing, like in python or ruby."
    (setq-local evilmi-always-simple-jump t))
  (add-hook 'python-mode-hook #'+evil|simple-matchit))


(def-package! evil-snipe
  :commands (evil-snipe-mode evil-snipe-override-mode
             evil-snipe-local-mode evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t
        evil-snipe-disabled-modes '(magit-mode elfeed-show-mode elfeed-search-mode)
        evil-snipe-aliases '((?\[ "[[{(]")
                             (?\] "[]})]")
                             (?\; "[;:]")))
  (add-hook 'doom-post-init-hook #'evil-snipe-mode)
  (add-hook 'doom-post-init-hook #'evil-snipe-override-mode))


(def-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


;; Without `evil-visualstar', * and # grab the word at point and search, no
;; matter what mode you're in. I want to be able to visually select a region and
;; search for other occurrences of it.
(def-package! evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (map! :v "*" #'evil-visualstar/begin-search-forward
        :v "#" #'evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode 1))

(unless MINIMAL-MODE (load! +extra))
