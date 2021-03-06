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
(evil-add-command-properties #'+jump/definition :jump t)
(evil-add-command-properties #'+jump/references :jump t)
(evil-add-command-properties #'counsel-etags-find-tag-at-point :jump t)

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

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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

;; disable it until this issue is solved
;; https://github.com/Kungsgeten/yankpad/issues/24
;; (use-package yankpad
;;   :commands (yankpad-expand)
;;   :config
;;   (setq yankpad-file (concat +showgood-dir "yankpad.org"))
;; )

(load! +bindings) ; my key bindings

(toggle-frame-maximized)

(require 'ox-latex)
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(require 'counsel-etags)
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; disable it since it cause lagging
;; (add-hook 'prog-mode-hook
;;   (lambda ()
;;     (add-hook 'after-save-hook
;;               'counsel-etags-virtual-update-tags 'append 'local)))

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

(require 'deadgrep)

(after! tldr
  (setq tldr-enabled-categories
        (append '("bb" "personal") tldr-enabled-categories))
  )

;; override printer to print json path in the way I want
(setq jsons-path-printer 'me/jsons-print-path-as-list)
