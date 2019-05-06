;;; my/org/config.el -*- lexical-binding: t; -*-

(load! "+babel")

;; these settings is for supporting generating diagrams in org mode
;; underneath using https://github.com/francoislaberge/diagrams to generate diagrams
;; buffer-file-name seems to be nil, not sure why
;; had to hard code the path
;; (add-to-list 'load-path (file-name-directory buffer-file-name))
(add-to-list 'load-path "~/.emacs.d/modules/my/org")
(setq ob-diagrams-cli-path "~/node_modules/.bin/diagrams")

;; https://www.reddit.com/r/emacs/comments/8kz8dv/tip_how_i_use_orgjournal_to_improve_my/
(def-package! org-journal
  :defer t
  ;; NOTE: :config won't work, need to use :custom
  ;; https://github.com/bastibe/org-journal/issues/9
  :custom
    (org-journal-dir "~/org/journal/2018/")
    (org-journal-file-format "%Y%m%d")
    (org-journal-date-format "%e %b %Y (%A)")
  ;; TODO: make company-dabbrev available globally
  :config
    (set-company-backend! 'org-journal-mode
        '(company-capf company-yasnippet company-dabbrev))
)

(def-package! org-noter
  :defer t
)

(def-package! ox-reveal
  :defer t
  :config
  (setq org-reveal-root (format "file://%s/reveal.js" (substitute-in-file-name "$HOME"))
        org-reveal-title-slide nil )
)

(after! org
    (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                                (sequence "⚑ WAITING(w)" "|")
                                (sequence "|" "✘ CANCELED(c)")))

    (setq org-agenda-files '("~/org/gtd/"
                            "~/org/Inbox.org" ))

    ;; https://emacs.stackexchange.com/questions/5889/how-to-highlight-text-permanently-in-org-mode
    (add-to-list 'org-emphasis-alist
        '("*" (:emphasis t :foreground "red")))

    ; Targets include this file and any file contributing to the agenda
    ; - up to 9 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9))))
    (setq org-refile-use-outline-path 'file)

    ;; prettify the exported table in HTML, add border and column divider etc
    (setq org-html-table-default-attributes '(:border "2" :rules "all" :frame "border"))

    (def-package! org-attach-screenshot
    :defer t
    :commands (org-attach-screenshot)
    :config
    (setq org-attach-screenshot-command-line
            "screencapture -i %f"

            org-attach-screenshot-dirfunction
            (lambda ()
            (concat +org-dir "/files/"))
            )
    )

    (when IS-MAC (require 'org-mac-link))
    (require 'ox-gfm nil t)
    (set-company-backend! 'org-mode '(company-dabbrev company-yasnippet company-capf))

    (evil-add-command-properties #'org-open-at-point :jump t)
    (add-to-list 'org-capture-templates
                 `("c" "Item to Current Clocked Task" item
                   (clock)
                   "%i%?" :empty-lines 1))
    (add-to-list 'org-capture-templates
                 `("C" "Contents to Current Clocked Task" plain
                   (clock)
                   "%i" :immediate-finish t :empty-lines 1))
    (add-to-list 'org-capture-templates
                 `("K" "Kill-ring to Current Clocked Task" plain
                   (clock)
                   "%c" :immediate-finish t :empty-lines 1))

    (add-to-list 'org-capture-templates
                 `("f" "Code Reference with Comments to Current Task"
                   plain (clock)
                   "%(ha/org-capture-code-snippet \"%F\")\n\n   %?"
                   :empty-lines 1))
    (add-to-list 'org-capture-templates
                 `("F" "Code Reference to Current Task"
                   plain (clock)
                   "%(ha/org-capture-code-snippet \"%F\")"
                   :empty-lines 1 :immediate-finish t))

    (add-to-list 'org-capture-templates
                 '("n" "Thought or Note"  entry
                   (file org-default-notes-file)
                   "* %?\n\n  %i\n\n  See: %a" :empty-lines 1))
)
