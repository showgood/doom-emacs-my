;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init (load "pdf-tools-autoloads" nil t)
  :config
  (pdf-tools-install)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  (setq-default pdf-view-display-size 'fit-page))

(use-package pdf-tools-org
  :load-path "~/.emacs.d/modules/lang/pdf"
  :commands (pdf-tools-org-export-to-org pdf-tools-org-import-from-org)
  :after (org)
)

(use-package org-pdfview
  :load-path "~/.emacs.d/modules/lang/pdf"
  :commands (org-pdfview-open)
  :after (org)
  :config
  (add-to-list 'org-file-apps
     '("\\.pdf\\'" . (lambda (file link)
      (org-pdfview-open link)))))

(use-package interleave
  :commands (interleave)
  :after (org)
)

(general-define-key
 :keymaps 'pdf-view-mode-map
 "g" '(pdf-view-first-page :which-key "first page")
 "G" '(noct:pdf-view-goto-page :which-key "go to page")
 "h" '(pdf-view-previous-page-command :which-key "previous page")
 "j" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
 "k" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
 "l" '(pdf-view-next-page-command :which-key "next page")
 "/" '(pdf-occur :which-key "pdf occur")
 "m" '(pdf-view-position-to-register "position to register")
 "'" '(pdf-view-jump-to-register :which-key "jump to register")
 "o" '(pdf-outline :which-key "pdf outline")
 "f" '(pdf-links-action-perform :which-key "links action")
 "b" '(pdf-view-midnight-minor-mode :which-key "midnight mode")
 "y" '(noct:pdf-view-page-as-text :which-key "text reflow")
 "q" '(image-kill-buffer :which-key "quit")
 "SPC" (general-simulate-keys "M-m" t)
 (kbd "C-o") '(pdf-history-backward :which-key "history backward")
 (kbd "C-i") '(pdf-history-forward :which-key "history forward")
 (kbd "C-s") '(isearch-forward :which-key "search forward")
 (kbd "C-r") '(isearch-backward :which-key "search backward")
 )

(general-define-key
:keymaps 'pdf-occur-buffer-mode-map
"<return>"  '(pdf-occur-goto-occurrence :which-key "go to occurrence")
)
