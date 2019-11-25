;;; my/cquery/config.el -*- lexical-binding: t; -*-


  ;; (use-package cquery
  ;;   :commands lsp
  ;;   :init (add-hook 'c-mode-hook #'cquery//enable)
  ;;         (add-hook 'c++-mode-hook #'cquery//enable))

(def-package! cquery
  :commands lsp-cquery-enable
  :init (add-hook 'c-mode-common-hook #'cquery//enable)
  :config
    (setq cquery-executable "/usr/local/bin/cquery")
    (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
    (setq cquery-cache-dir "~/.cquerycache/cquery_cached_index")
    ;; initialization options
    (setq cquery-extra-init-params '(:index (:blacklist (".*/vendor/nvidia/.*" ".*\.s")) :diagnostics (:blacklist (".*\.s")) :discoverSystemIncludes :json-false))
  )

;; setup as needed for your env/projects
;; also see lsp-project-whitelist lsp-project-blacklist cquery-root-matchers
;; (setq cquery-project-roots '("~/repo" ))
;; (setq cquery-executable "/usr/local/bin/cquery")
