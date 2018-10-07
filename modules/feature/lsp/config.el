;;; feature/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-ignore-duplicate t
        ; don't include type signature in the child frame
        lsp-ui-doc-include-signature nil
        ; don't show symbol on the right of info
        lsp-ui-sideline-show-symbol nil
  )

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(def-package! company-lsp
  :config
    (setq company-lsp-enable-recompletion t)
    (push 'company-lsp company-backends)
)
