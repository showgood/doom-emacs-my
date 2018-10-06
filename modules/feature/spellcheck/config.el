;;; feature/spellcheck/config.el -*- lexical-binding: t; -*-

(def-package! flyspell ; built-in
  :commands flyspell-mode
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-list-command "--list"
        ispell-really-hunspell t
        ispell-extr-args '("--dont-tex-check-comments")))


(def-package! flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :config
  (cond ((featurep! :completion helm)
         (require 'flyspell-correct-helm))
        ((featurep! :completion ivy)
         (require 'flyspell-correct-ivy))
        (t
         (require 'flyspell-correct-popup)
         (setq flyspell-popup-correct-delay 0.8)
         (define-key popup-menu-keymap [escape] #'keyboard-quit))))


(def-package! langtool
  :init
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.1/libexec/languagetool-commandline.jar"
        langtool-mother-tongue "en"
        langtool-default-language "en-US"
        langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES")))
