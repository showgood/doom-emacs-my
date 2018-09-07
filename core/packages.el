;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core-os.el
;; In case this config is shared across multiple computers (like mine is), let's
;; protect these from autoremoval.
(package! exec-path-from-shell :ignore (not IS-MAC))
(package! osx-clipboard        :ignore (not IS-MAC))

;; core-ui.el
(package! all-the-icons)
(unless (boundp 'display-line-numbers)
  (package! nlinum)
  (package! nlinum-hl)
  (package! nlinum-relative))
(package! rainbow-delimiters)

(unless MINIMAL-MODE
  (package! highlight-indentation)
  (package! highlight-numbers)
  (package! visual-fill-column)
  (package! fringe-helper)
  (package! editorconfig)
  (package! expand-region)
  (package! command-log-mode)
  (package! smart-forward)
  (package! undo-tree)
  (package! pcre2el)
)

;; core-popups.el
(package! shackle)

;; core-editor.el
(package! ace-link)
(package! ace-window)
(package! avy)

;; (package! help-fns+ :recipe (:fetcher github :repo "emacsmirror/help-fns-plus"))
(package! bookmark+ :recipe (:fetcher github :repo "emacsmirror/bookmark-plus"))

(package! smartparens)
(package! wgrep)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! which-key)
(package! hydra)
(package! general)
(package! helpful)
