(def-package! slime
  :defer t
  :config
  (setq inferior-lisp-program "clisp")
  (require 'slime-fuzzy))


(def-package! macrostep
  :commands macrostep-expand
  :config
  (map! :map macrostep-keymap
        :n "RET"    #'macrostep-expand
        :n "e"      #'macrostep-expand
        :n "u"      #'macrostep-collapse
        :n "c"      #'macrostep-collapse

        :n "TAB"    #'macrostep-next-macro
        :n "n"      #'macrostep-next-macro
        :n "J"      #'macrostep-next-macro

        :n "S-TAB"  #'macrostep-prev-macro
        :n "K"      #'macrostep-prev-macro
        :n "p"      #'macrostep-prev-macro

        :n "q"      #'macrostep-collapse-all
        :n "C"      #'macrostep-collapse-all)
  ;; `evil-normalize-keymaps' seems to be required for macrostep or it won't
  ;; apply for the very first invocation
  (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps))

(def-package! flycheck-cask
  :when (featurep! :feature syntax-checker)
  :commands flycheck-cask-setup
  :init
  (add-hook! 'emacs-lisp-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t)))


(def-package! overseer
  :commands overseer-test
  :init (set! :popup "*overseer*" :size 12))


(use-package lispy
  :init
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
)

(use-package lispyville
  :init
    (add-hook 'lispy-mode-hook #'lispyville-mode)
)

;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'lispy-mode-hook #'lispyville-mode)
