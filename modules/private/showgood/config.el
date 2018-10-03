;; -*- origami-fold-style: triple-braces -*-

;; do NOT turn on it
;; if a search in evil (using /) failed to find match,
;; with debug-on-error set to t will trigger debugger
;; and after that evil will behave really weird. for eg:
;; d (instead of dd) will delete the whole line
;; also using v to select a region and yank will alway copy the whole
;; line, not just the selected region. basically evil will be unusable
;; once it gets into that state.
;; (setq debug-on-error t)

(load! +alias)  ; emacs alias

(load! +commands)  ; my custom ex commands
(load! +myabbrev)
(load! site-lisp/ox-reveal)

; proper line wrapping
(global-visual-line-mode 1)

(evil-add-command-properties #'counsel-imenu :jump t)

;; ==== frequently used packages {{{ ====
(require 'hl-anything)
(hl-highlight-mode)

(require 'origami)
(global-origami-mode 1)
;; ==== END frequently used packages }}} ====

(require 'vlf)
(require 'vlf-setup)

(setq +org-dir (concat (substitute-in-file-name "$HOME/") "org"))

(defvar +showgood-dir (file-name-directory load-file-name))
(defvar +showgood-snippets-dir (expand-file-name "snippets/" +showgood-dir))

(after! smartparens
  ;; auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "ret") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "ret") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

(after! evil-mc
  ;; if i'm in insert mode, chances are i want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+showgood-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

;; ==== world clock {{{ ====
;; https://en.wikipedia.org/wiki/list_of_tz_database_time_zones
(setq display-time-world-list
      '(("America/New_York" "New York")
        ("Asia/Shanghai" "Shanghai")
        ("Australia/Sydney" "Sydney")
        ("Europe/London" "London")
        ("Europe/Berlin" "Germany")
        ("Europe/Rome" "Italy")
        ("Europe/Paris" "Paris")))

;; quick way to dispaly world time clock
(defalias 'wc 'display-time-world)
;; ==== end world clock }}} ====

(require 'eacl)

;; ==== flycheck settings {{{ ====
;; (setq flycheck-c/c++-clang-executable "/opt/bb/bin/clang++")
(setq flycheck-c/c++-clang-executable "/usr/local/opt/llvm/bin/clang++")
(setq flycheck-clang-args '("-m32" "-Dlint" "-D_REENTRANT"
                            "-D_THREAD_SAFE" "-DBB_THREADED" "-DBSL_OVERRIDES_STD"))

;; ==== flycheck settings }}} ====

(setq eshell-aliases-file (concat +showgood-dir "eshell_alias"))

;; support large file size
(require 'tramp)
(setq tramp-inline-compress-start-size 10000000)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; http://emacs.stackexchange.com/questions/27/how-can-i-use-my-local-emacs-client-as-the-editor-for-remote-machines-i-access
;; (require 'with-editor)

;; (add-hook 'shell-mode-hook  'with-editor-export-editor)
;; (add-hook 'term-mode-hook   'with-editor-export-editor)
;; (add-hook 'eshell-mode-hook 'with-editor-export-editor)

;; ==== ediff settings {{{ ====
(require 'evil-ediff)
;; https://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

; ignore white space
(csetq ediff-diff-options "-w")
;; ==== end ediff settings }}} ====

(autoload 'dash-at-point "dash-at-point"
          "search the word at point with dash." t nil)

;; ==== beacon settings {{{ ====
(require 'beacon)
(beacon-mode 1)
(setq beacon-color "#66cd00")
(setq beacon-size 50)
(setq beacon-blink-delay 0.7)
;; ==== end beacon settings }}} ====

(use-package fancy-narrow
  :commands (fancy-narrow-to-region
             fancy-widen)
)

;; ==== deft settings {{{ ====
(require 'deft)
(setq deft-default-extension "org")
(setq deft-extensions '("org"))
(setq deft-directory "~/org")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-file-naming-rules '((noslash . "-")
                               (nospace . "-")
                               (case-fn . downcase)))
(setq deft-text-mode 'org-mode)
(add-to-list 'evil-emacs-state-modes 'deft-mode)
;; ==== end deft settings }}} ====

(use-package tldr
  :commands (tldr)
)

(use-package visual-regexp
  :commands (vr/query-replace))

(use-package visual-regexp-steroids
  :commands (vr/select-query-replace))

(require 'atomic-chrome)
(atomic-chrome-start-server)

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "/usr/local/bin/clisp")
(require 'slime-autoloads)
;; (require 'slime)
;; (slime-setup '(slime-fancy slime-tramp slime-asdf))
;; (slime-setup '(slime-fancy slime-tramp))
;; (slime-require :swank-listener-hooks)

(use-package yankpad
  :commands (yankpad-expand)
  :config
  (setq yankpad-file (concat +showgood-dir "yankpad.org"))
)

(load! +bindings) ; my key bindings

(toggle-frame-maximized)

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

(require 'langtool)
(setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.1/libexec/languagetool-commandline.jar"
      langtool-mother-tongue "nl"
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))

(when IS-MAC
    (require 'org-mac-link)
)

(require 'ox-latex)
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; prevent elpy because too sluggish
;; (setq eldoc-idle-delay 2)

(setq org-table-convert-region-max-lines 5000)

(require 'counsel-etags)
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; Setup auto update now
(add-hook 'prog-mode-hook
  (lambda ()
    (add-hook 'after-save-hook
              'counsel-etags-virtual-update-tags 'append 'local)))

(setq org-ditaa-jar-path "~/tools/ditaa0_9.jar")
(setq org-plantuml-jar-path
      (expand-file-name "~/tools/plantuml.1.2018.5.jar"))

(require 'ox-reveal)
(setq org-reveal-root (format "file://%s/reveal.js" (substitute-in-file-name "$HOME")))
(setq org-reveal-title-slide nil)

;; ONLY turn on this when local repository for package needs to be updated
;; then run: M-x elpamr-create-mirror-for-installed
(require 'elpa-mirror)
(setq elpamr-default-output-directory "~/myelpa")

(setq company-lsp-enable-recompletion t)

;;; private/my-cc/autoload.el -*- lexical-binding: t; -*-
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(defun ccls/base () (interactive) (lsp-ui-peek-find-custom 'base "$ccls/base"))
(defun ccls/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$ccls/callers"))
(defun ccls/vars (kind) (lsp-ui-peek-find-custom 'vars "$ccls/vars" (plist-put (lsp--text-document-position-params) :kind kind)))
(defun ccls/bases ()
  (interactive)
  (lsp-ui-peek-find-custom 'base "$ccls/inheritanceHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t :level 3))))
(defun ccls/derived ()
  (interactive)
  (lsp-ui-peek-find-custom 'derived "$ccls/inheritanceHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t :level 3 :derived t))))
(defun ccls/members ()
  (interactive)
  (lsp-ui-peek-find-custom 'base "$ccls/memberHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t))))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 128))))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 64))))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:excludeRole 32))))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom
   'read "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 8))))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom
   'write "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 16))))

;; xref-find-apropos (workspace/symbol)

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
      (while (string-match pattern line i)
        (setq i (match-end 0))
        (add-face-text-property (match-beginning 0) (match-end 0) 'isearch t line)
        )
      line)))

(with-eval-after-load 'lsp-methods
  ;;; Override
  ;; This deviated from the original in that it highlights pattern appeared in symbol
  (defun lsp--symbol-information-to-xref (pattern symbol)
    "Return a `xref-item' from SYMBOL information."
    (let* ((location (gethash "location" symbol))
           (uri (gethash "uri" location))
           (range (gethash "range" location))
           (start (gethash "start" range))
           (name (gethash "name" symbol)))
      (xref-make (format "[%s] %s"
                         (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                         (my/highlight-pattern-in-text (regexp-quote pattern) name))
                 (xref-make-file-location (string-remove-prefix "file://" uri)
                                          (1+ (gethash "line" start))
                                          (gethash "character" start)))))

  (cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
    (let ((symbols (lsp--send-request (lsp--make-request
                                       "workspace/symbol"
                                       `(:query ,pattern)))))
      (mapcar (lambda (x) (lsp--symbol-information-to-xref pattern x)) symbols)))
  )

(setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame
(setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info

(define-key evil-normal-state-map (kbd "C-p") 'lsp-ui-peek-jump-forward)
(define-key evil-normal-state-map (kbd "C-t") 'lsp-ui-peek-jump-backward)

(use-package anki-editor
  :ensure t)

(setq org-export-allow-bind-keywords t)
