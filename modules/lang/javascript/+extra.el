;;; lang/javascript/+extra.el -*- lexical-binding: t; -*-

(def-package! nodejs-repl :commands nodejs-repl)

(def-package! tern
  :hook (js2-mode . tern-mode)
  :config
  (advice-add #'tern-project-dir :override #'doom-project-root))


(def-package! company-tern
  :when (featurep! :completion company)
  :after tern
  :config
  (set! :company-backend 'js2-mode '(company-tern)))


(def-package! rjsx-mode
  :commands rjsx-mode
  :mode "\\.jsx$"
  :mode "components/.+\\.js$"
  :init
  (defun +javascript-jsx-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))

  (push (cons #'+javascript-jsx-file-p 'rjsx-mode) magic-mode-alist)

  :config
  (set! :electric 'rjsx-mode :chars '(?\} ?\) ?. ?>))

  ;; disable electric keys (I use snippets and `emmet-mode' instead)
  (map! :map rjsx-mode-map
        "<" nil
        "C-d" nil)
  (add-hook! rjsx-mode
    ;; jshint doesn't really know how to deal with jsx
    (push 'javascript-jshint flycheck-disabled-checkers)))


(def-package! coffee-mode
  :mode "\\.coffee$"
  :init (setq coffee-indent-like-python-mode t))


(def-package! web-beautify
  :commands web-beautify-js
  :init
  (map! :map* (json-mode js2-mode-map) :n "gQ" #'web-beautify-js))


(def-package! eslintd-fix
  :commands (eslintd-fix-mode eslintd-fix))


;;
;; Skewer-mode
;;

(def-package! skewer-mode
  :commands (skewer-mode run-skewer)
  :config
  (map! :map skewer-mode-map
        :localleader
        :n "sE" #'skewer-eval-last-expression
        :n "se" #'skewer-eval-defun
        :n "sf" #'skewer-load-buffer))

(def-package! skewer-css ; in skewer-mode
  :commands skewer-css-mode
  :config
  (map! :map skewer-css-mode-map
        :localleader
        :n "se" #'skewer-css-eval-current-declaration
        :n "sr" #'skewer-css-eval-current-rule
        :n "sb" #'skewer-css-eval-buffer
        :n "sc" #'skewer-css-clear-all))

(def-package! skewer-html ; in skewer-mode
  :commands skewer-html-mode
  :config
  (map! :map skewer-html-mode-map
        :localleader
        :n "se" #'skewer-html-eval-tag))


;;
;; Projects
;;

(def-project-mode! +javascript-screeps-mode
  :match "/screeps\\(-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! +screeps))

(def-project-mode! +javascript-gulp-mode
  :files "gulpfile.js")

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode js2-mode markdown-mode)
  :files "package.json"
  :on-enter
  (when (make-local-variable 'exec-path)
    (push (doom-project-expand "node_modules/.bin")
          exec-path)))


;;
;; Tools
;;

(def-project-mode! +javascript-eslintd-fix-mode
  :add-hooks (eslintd-fix-mode))

