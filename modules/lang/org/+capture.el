;;; lang/org/+capture.el -*- lexical-binding: t; -*-

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture (whether or not Emacs is open/running),
;;    like, say, from qutebrowser, vimperator, dmenu or a global keybinding.

(defvar +org-default-todo-file "todo.org"
  "TODO")

(defvar +org-default-notes-file "notes.org"
  "TODO")

;; (defvar org-capture-templates
;;   '(("t" "Todo" entry
;;      (file+headline +org-default-todo-file "Inbox")
;;      "* [ ] %?\n%i" :prepend t :kill-buffer t)

;;     ("n" "Notes" entry
;;      (file+headline +org-default-notes-file "Inbox")
;;      "* %u %?\n%i" :prepend t :kill-buffer t)))

(after! org
  (defvaralias 'org-default-notes-file '+org-default-notes-file)

  (setq org-default-notes-file (expand-file-name +org-default-notes-file +org-dir))

  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame)

  (when (featurep! :feature evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state))

  (when (featurep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))

(defvar org-capture-templates
  '(("t" "todo" entry (file "Inbox.org")
               "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n")
              ("r" "respond" entry (file "Inbox.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
              ("n" "note" entry (file "Inbox.org")
               "* %? :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n")
              ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
               "* %?\n%U\n")
              ;; ("w" "org-protocol" entry (file "~/org/Inbox.org")
              ;;  "* TODO Review %c\n%U\n" :immediate-finish t)
              ("w" "Web site" entry
               (file "")
               "* %a :website:\n\n%U %?\n\n%:initial")
              ("m" "Meeting" entry (file "Inbox.org")
               "* MEETING with %? :MEETING:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
              ("a" "Appointment" entry (file "Inbox.org")
               "* Appointment with %? :APPOINTMENT:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
              ("p" "Phone call" entry (file "Inbox.org")
               "* PHONE %? :PHONE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
              ("c" "Contacts" entry (file "contacts.org")
                "* %(org-contacts-template-name) \n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:\n")
              ("l" "Link" entry
                 (file "~/org/rss.org")
                 "* %a\n%U")
              ("h" "Habit" entry (file "Inbox.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
