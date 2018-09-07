(def-package! disaster :commands disaster)

;; Major modes
;;

(def-package! cmake-mode
  :mode "/CMakeLists\\.txt$"
  :mode "\\.cmake\\$"
  :config
  (set! :company-backend 'cmake-mode '(company-cmake company-yasnippet)))

(def-package! cuda-mode :mode "\\.cuh?$")

(def-package! opencl-mode :mode "\\.cl$")

(def-package! demangle-mode
  :hook llvm-mode)

(def-package! glsl-mode
  :mode "\\.glsl$"
  :mode "\\.vert$"
  :mode "\\.frag$"
  :mode "\\.geom$")

;;
;; Company plugins
;;

(def-package! company-cmake
  :when (featurep! :completion company)
  :after cmake-mode)

(def-package! company-glsl
  :when (featurep! :completion company)
  :after glsl-mode
  :config
  (if (executable-find "glslangValidator")
      (warn "glsl-mode: couldn't find glslangValidator, disabling company-glsl")
    (set! :company-backend 'glsl-mode '(company-glsl))))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "llvm")
  )

(defun +ccls//enable ()
  (condition-case nil
      (lsp-ccls-enable)
    (user-error nil)))
;; (global-set-key (kbd "c-c i") 'clang-format-region)
;; (global-set-key (kbd "c-c u") 'clang-format-buffer)

(def-package! ccls
  :after cc-mode
  :init (add-hook! (c-mode c++-mode objc-mode) #'+ccls//enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq ccls-extra-init-params
        '(:completion
          (
           :detailedLabel t
           :includeBlacklist
           ("^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
            "^/usr/(local/)?include/c\\+\\+/v1/"
            ))
          :diagnostics (:frequencyMs 5000)
          :index (:reparseForDependency 1)))

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (set! :company-backend '(c-mode c++-mode objc-mode) 'company-lsp)
  )
