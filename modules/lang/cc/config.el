;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(defvar +cc-default-include-paths (list "include/")
  "A list of default paths, relative to a project root, to search for headers in
C/C++. Paths can be absolute. This is ignored if your project has a JSON
compilation database.")

(defvar +cc-default-compiler-options
  `((c-mode . nil)
    (c++-mode
     . ,(list "-std=c++11" ; use C++11 by default
              (when IS-MAC
                ;; NOTE beware: you'll get abi-inconsistencies when passing
                ;; std-objects to libraries linked with libstdc++ (e.g. if you
                ;; use boost which wasn't compiled with libc++)
                (list "-stdlib=libc++"))))
    (objc-mode . nil))
  "A list of default compiler options for the C family. These are ignored if a
compilation database is present in the project.")

;; Plugins

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm" . objc-mode)
  :preface
  (defun +cc-c++-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (or (file-exists-p (expand-file-name
                             (concat (file-name-sans-extension buffer-file-name)
                                     ".cpp")))
             (when-let* ((file (car-safe (projectile-get-other-files
                                          buffer-file-name
                                          (projectile-current-project-files)))))
               (equal (file-name-extension file) "cpp")))))

  (defun +cc-objc-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))

  (push (cons #'+cc-c++-header-file-p  'c++-mode)  magic-mode-alist)
  (push (cons #'+cc-objc-header-file-p 'objc-mode) magic-mode-alist)

  :init
  (setq-default c-basic-offset tab-width)

  :config
  (set! :electric '(c-mode c++-mode objc-mode java-mode)
    :chars '(?\n ?\}))
  ;; (set! :company-backend
  ;;   '(c-mode c++-mode objc-mode)
  ;;   '(company-irony-c-headers company-irony))

;;; Style/formatting
  ;; C/C++ style settings
  ;; (c-toggle-electric-state -1)
  ;; (c-toggle-auto-newline -1)
  ;; (c-set-offset 'substatement-open '0)  ; don't indent brackets
  ;; (c-set-offset 'inline-open       '+)
  ;; (c-set-offset 'block-open        '+)
  ;; (c-set-offset 'brace-list-open   '+)
  ;; (c-set-offset 'case-label        '+)
  ;; (c-set-offset 'access-label      '-)
  ;; (c-set-offset 'arglist-intro     '+)
  ;; (c-set-offset 'arglist-close     '0)
  (c-set-offset 'innamespace 0)
  ;; Indent privacy keywords at same level as class properties
  ;; (c-set-offset 'inclass #'+cc-c-lineup-inclass)

;;; Better fontification (also see `modern-cpp-font-lock')
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)

  (unless MINIMAL-MODE
    (add-hook! (c-mode c++-mode) #'highlight-numbers-mode))

  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)

  ;; Improve indentation of inline lambdas in C++11
  (advice-add #'c-lineup-arglist :around #'+cc*align-lambda-arglist)

;;; Keybindings
  ;; Completely disable electric keys because it interferes with smartparens and
  ;; custom bindings. We'll do this ourselves.
  (setq c-tab-always-indent nil
        c-electric-flag nil)
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")"))
    (define-key c-mode-base-map key nil))
  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (map! :map c++-mode-map
        "<" nil
        :i ">" #'+cc/autoclose->-maybe)

  ;; ...and leave it to smartparens
  (after! smartparens
    (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
      (sp-local-pair "<" ">" :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p))
      (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      ;; Doxygen blocks
      (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
      (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))
    )
  )

(def-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(unless MINIMAL-MODE (load! +extra))

(when (featurep! :feature lsp)
  (load! +ccls))
