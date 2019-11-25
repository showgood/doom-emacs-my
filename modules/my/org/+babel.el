;;; my/org/+babel.el -*- lexical-binding: t; -*-

;; This is a mod based on +babel.el from doom-emacs module
;; I need to do this because emacs-jupyter package requries
;; set the `org-babel-load-languages' specifically.
;; It requires that python must already be loaded and
;; jupyter has to be put at last otherwise jupyter-python
;; won't work.
;; doom-emacs uses lazy-load and a language won't be added
;; to `org-babel-load-languages' until a block of that
;; language is executed for the first time.
;;
;; Also since I use `jupyter-emacs' package, I don't need
;; `ob-ipython' anymore.

(add-hook 'org-load-hook #'+org|init-babel)

(defvar +org-babel-mode-alist
  '((cpp . C)
    (C++ . C)
    (D . C)
    (sh . shell)
    (bash . shell)
    (matlab . octave))
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

(defvar +org-babel-load-functions ()
  "A list of functions for loading the current executing src block. They take
one argument (the language specified in the src block, as a string). Stops at
the first function to return non-nil.")

(defun +org|init-babel ()
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      ;; must put jupyter at last
      (jupyter . t)))

  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil) ; you don't need my permission

  (defun +org*babel-lazy-load-library (info)
    "Load babel libraries as needed when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (if (symbolp lang) lang (intern lang)))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (when (and (not (cdr (assq lang org-babel-load-languages)))
                 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                     (require (intern (format "ob-%s" lang)) nil t)))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t))
  (advice-add #'org-babel-confirm-evaluate :after-while #'+org*babel-lazy-load-library)

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; `org-babel-get-header' was removed from org in 9.0. Quite a few babel
  ;; plugins use it, so until those plugins update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p)))

(def-package! jupyter
  :defer t
  :init
  (after! ob-async
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-python"))
)
