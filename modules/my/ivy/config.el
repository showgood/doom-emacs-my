;;; my/ivy/config.el -*- lexical-binding: t; -*-

(after! ivy
  (ivy-add-actions
   'counsel-fzf
   '(("r" prot/counsel-fzf-dir "change root directory")
     ("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("a" prot/counsel-fzf-ace-window "ace-window switch")))

  (ivy-add-actions
   'counsel-rg
   '(("r" prot/counsel-rg-dir "change root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  (ivy-add-actions
   'counsel-find-file
   '(("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  (setq ivy-count-format "(%d/%d) "
        ;; http://oremacs.com/2017/11/30/ivy-0.10.0/
        ivy-use-selectable-prompt t)

  ;; http://oremacs.com/2017/04/09/ivy-0.9.0/
  (setq counsel-yank-pop-separator "\n-------------------------------------------------------\n")
)

(after! ivy-posframe
  (setq ivy-posframe-parameters
   '((left-fringe . 2)
     (right-fringe . 2)
     (internal-border-width . 2)))

(setq ivy-posframe-height-alist
   '((swiper . 15)
     (swiper-isearch . 15)
     (t . 10))

  ivy-posframe-display-functions-alist
   '((complete-symbol . ivy-posframe-display-at-point)
     (swiper . nil)
     (swiper-isearch . nil)
     (t . ivy-posframe-display-at-frame-center))))
