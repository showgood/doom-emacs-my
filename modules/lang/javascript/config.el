;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(def-package! js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil)

  (add-hook! 'js2-mode-hook
    #'(flycheck-mode highlight-indentation-mode rainbow-delimiters-mode))

  (set! :repl 'js2-mode #'+javascript/repl)
  (set! :electric 'js2-mode :chars '(?\} ?\) ?.))
  (set! :jump 'js2-mode :xref-backend #'xref-js2-xref-backend)

  ;; Conform switch-case indentation to js2 normal indent
  (defvaralias 'js-switch-indent-offset 'js2-basic-offset)

  (sp-with-modes '(js2-mode rjsx-mode)
    (sp-local-pair "/* " " */" :post-handlers '(("| " "SPC"))))

  ;; If it's available globally, use eslint_d
  (setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))

  (defun +javascript|init-flycheck-eslint ()
    "Favor local eslint over global installs and configure flycheck for eslint."
    (when (derived-mode-p 'js-mode)
      (when-let* ((exec-path (list (doom-project-expand "node_modules/.bin")))
                  (eslint (executable-find "eslint")))
        (setq-local flycheck-javascript-eslint-executable eslint))
      (when (flycheck-find-checker-executable 'javascript-eslint)
        ;; Flycheck has it's own trailing command and semicolon warning that was
        ;; conflicting with the eslint settings.
        (setq-local js2-strict-trailing-comma-warning nil)
        (setq-local js2-strict-missing-semi-warning nil))))
  (add-hook 'flycheck-mode-hook #'+javascript|init-flycheck-eslint)

  (map! :map js2-mode-map
        :localleader
        "r" #'+javascript/refactor-menu
        "S" #'+javascript/skewer-this-buffer))


;; A find-{definition,references} backend for js2-mode. NOTE The xref API is
;; unstable and may break with an Emacs update.
(def-package! xref-js2 :commands xref-js2-xref-backend)


(def-package! js2-refactor
  :commands
  (js2r-extract-function js2r-extract-method js2r-introduce-parameter
   js2r-localize-parameter js2r-expand-object js2r-contract-object
   js2r-expand-function js2r-contract-function js2r-expand-array
   js2r-contract-array js2r-wrap-buffer-in-iife js2r-inject-global-in-iife
   js2r-add-to-globals-annotation js2r-extract-var js2r-inline-var
   js2r-rename-var js2r-var-to-this js2r-arguments-to-object js2r-ternary-to-if
   js2r-split-var-declaration js2r-split-string js2r-unwrap js2r-log-this
   js2r-debug-this js2r-forward-slurp js2r-forward-barf)
  :init
  (def-menu! +javascript/refactor-menu
    "Refactoring commands for `js2-mode' buffers."
    '(("Extract into function"           :exec js2r-extract-function          :region t)
      ("Extract into method"             :exec js2r-extract-method            :region t)
      ("Introduce parameter to function" :exec js2r-introduce-parameter       :region t)
      ("Localize parameter"              :exec js2r-localize-parameter        :region nil)
      ("Expand object"                   :exec js2r-expand-object             :region nil)
      ("Expand function"                 :exec js2r-expand-function           :region nil)
      ("Expand array"                    :exec js2r-expand-array              :region nil)
      ("Contract object"                 :exec js2r-contract-object           :region nil)
      ("Contract function"               :exec js2r-contract-function         :region nil)
      ("Contract array"                  :exec js2r-contract-array            :region nil)
      ("Wrap buffer in IIFE"             :exec js2r-wrap-buffer-in-iife       :region nil)
      ("Inject global into IIFE"         :exec js2r-inject-global-in-iife     :region t)
      ("Add to globals annotation"       :exec js2r-add-to-globals-annotation :region nil)
      ("Extract variable"                :exec js2r-extract-var               :region t)
      ("Inline variable"                 :exec js2r-inline-var                :region t)
      ("Rename variable"                 :exec js2r-rename-var                :region nil)
      ("Replace var with this"           :exec js2r-var-to-this               :region nil)
      ("Arguments to object"             :exec js2r-arguments-to-object       :region nil)
      ("Ternary to if"                   :exec js2r-ternary-to-if             :region nil)
      ("Split var declaration"           :exec js2r-split-var-declaration     :region nil)
      ("Split string"                    :exec js2r-split-string              :region nil)
      ("Unwrap"                          :exec js2r-unwrap                    :region t)
      ("Log this"                        :exec js2r-log-this)
      ("Debug this"                      :exec js2r-debug-this)
      ("Reformat buffer (eslint_d)"      :exec eslintd-fix :region nil :when (fboundp 'eslintd-fix)))
    :prompt "Refactor: "))

(unless MINIMAL-MODE (load! +extra))
