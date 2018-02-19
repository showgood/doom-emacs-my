;;; lang/python/config.el -*- lexical-binding: t; -*-

(defvar +python-pyenv-root nil
  "The path to pyenv's root directory. This is automatically set when `python'
is loaded.")

(defvar +python-pyenv-versions nil
  "Available versions of python in pyenv.")

(defvar-local +python-current-version nil
  "The currently active pyenv version.")

(def-package! pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))

(def-package! python
  :commands python-mode
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python")
  :config
  (add-hook! 'python-mode-hook #'(flycheck-mode highlight-numbers-mode))

  (set! :repl 'python-mode #'+python/repl)
  (set! :electric 'python-mode :chars '(?:))

  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

  ;; Version management with pyenv
  (defun +python|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if +python-current-version
              (format "Python %s" +python-current-version)
            "Python")))
  (add-hook 'python-mode-hook #'+python|add-version-to-modeline)

  (if (not (executable-find "pyenv"))
      (setq +python-current-version (string-trim (shell-command-to-string "python --version 2>&1 | cut -d' ' -f2")))
    (setq +python-pyenv-root     (string-trim (shell-command-to-string "pyenv root"))
          +python-pyenv-versions (split-string (shell-command-to-string "pyenv versions --bare") "\n" t))

    (defun +python|detect-pyenv-version ()
      "Detect the pyenv version for the current project and set the relevant
environment variables."
      (when-let* ((version-str (shell-command-to-string "python --version 2>&1 | cut -d' ' -f2")))
        (setq version-str (string-trim version-str)
              +python-current-version version-str)
        (let ((pyenv-current-path (concat +python-pyenv-root "/versions/" version-str)))
          (when (file-directory-p pyenv-current-path)
            (setq pythonic-environment pyenv-current-path)))
        (when (member version-str +python-pyenv-versions)
          (setenv "PYENV_VERSION" version-str))))
    (add-hook 'python-mode-hook #'+python|detect-pyenv-version))

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (after! smartparens
    (sp-with-modes 'python-mode
    (sp-local-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p)))
    )
  )

(require 'elpy)
(elpy-enable)

;; NOTE: do NOT set to jupyter, otherwise ob-ipython would break
;; set to ipython
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt")

(setq elpy-rpc-python-command (format "/Users/%s/anaconda2/bin/python" user-login-name))

(general-define-key
 :prefix ","
 :states '(normal visual)
 :keymaps 'elpy-mode-map
 "d" '(elpy-goto-definition :which-key "elpy-goto-definition")
 "D" '(elpy-doc :which-key "elpy-doc")
 "f" '(elpy-format-code :which-key "elpy-format-code")
 "r" '(xref-find-references :which-key "xref-find-references")
 "m" '(elpy-multiedit :which-key "elpy-multiedit")
 "M" '(elpy-multiedit-stop :which-key "elpy-multiedit-stop")
 "t" '(elpy-test :which-key "elpy-test")
)
