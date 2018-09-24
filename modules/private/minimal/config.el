;(setq debug-on-error t)
(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "M-m"
 "/" '(counsel-rg :wich-key "rg")
 "TAB" '(switch-to-previous-buffer :which-key "prev buffer")
 "SPC" '(counsel-M-x :which-key "M-x")
 "k" '(evil-avy-goto-char-2 :which-key "jump char 2")
 "q" '(persp-switch-to-buffer :which-key "Switch workspace buffer")
 "Q" '(switch-to-buffer :which-key "Switch to buffer")
 "d" '(counsel-git-grep :which-key "git grep")
 "RET" '(bookmark-jump :which-key "Jump to bookmark")

 "a" '(:ignore t :which-key "applications")
 "at" '(+term/open :which-key "+term/open")

 "b" '(:ignore t :which-key "buffers")
 "bb" '(persp-switch-to-buffer :which-key "Switch workspace buffer")
 "bB" '(switch-to-buffer :which-key "Switch to buffer")
 "br" '(rename-buffer :which-key "rename buffer")
 "bk" '(doom/kill-this-buffer :which-key "kill buffer")
 "bs" '(open-scratch :which-key "open scratch")
 "bt" '(me/switch-to-project-term :which-key "open project terminal")

 "f" '(:ignore t :which-key "Files/Fold")
 "fd" '(+evil:delete-this-file :which-key "delete this file")
 "ff" '(counsel-find-file :which-key "find file")
 "fj" '(dired-jump :which-key "dired jump")
 "fn" '(cp-filename-of-current-buffer :which-key "yank filename only")
 "fp" '(+hlissner/yank-buffer-filename :which-key "yank file full path")
 "fo" '(hydra-folding/body :which-key "hydra folding")
 "fr" '(counsel-recentf :which-key "recent file")

 "g" '(:ignore t :which-key "Git")
 "gs" '(magit-status :which-key "Git status")
 "ga" '(magit-stage-file :which-key "stage this file")
 "gb" '(magit-blame :which-key "Git blame")
 "gc" '(magit-commit :which-key "Git commit")
 "gd" '(magit-diff-buffer-file :which-key "Git diff")
 ;; list commits affect current function
 "gf" '(magit-log-trace-definition :which-key "show commits for this function")
 ;; list commits affect current file
 "gl" '(magit-log-buffer-file :which-key "show commits for this file")
 "gg" '(hydra-git/body :which-key "Git gutter")
 "gp" '(magit-push-current :which-key "Git push")
 "gt" '(my-git-timemachine :which-key "Git time machine")

 "h" '(:ignore t :which-key "Help/Highlight")
 "hh" '(helpful-at-point :which-key "helpful-at-point")
 "hm" '(describe-mode :which-key "Describe mode")
 "hf" '(helpful-function :which-key "Describe function")
 "hk" '(helpful-key :which-key "Describe key")
 "hv" '(helpful-variable :which-key "Describe variable")
 "hL" '(hl-highlight-thingatpt-global :which-key "highlight global")
 "hl" '(hl-highlight-thingatpt-local :which-key "highlight local")
 "hu" '(hl-unhighlight-all-local :which-key "un highlight local")
 "hU" '(hl-unhighlight-all-global :which-key "un highlight global")

 "j" '(:ignore t :which-key "Jump")
 "ji" '(imenu :which-key "Imenu")
 "jb" '(avy-pop-mark :which-key "jump back")
 "jI" '(imenu-anywhere :which-key "Imenu across buffers")

 "l" '(:ignore t :which-key "workspace/layout")
 "ln" '(+workspace/me/new :which-key "New workspace")
 "ll" '(+workspace/switch-to :which-key "switch workspace")
 "l TAB" '(doom/jump-to-last-workspace :which-key "toggle workspace")

 "o" '(:ignore t :which-key "bookmark")
 "om" '(bookmark-set :which-key "set bookmark")
 "ol" '(bookmark-bmenu-list :which-key "open bookmark buffer")
 "ou" '(bmkp-url-target-set :which-key "set url bookmark")
 "os" '(bmkp-set-snippet-bookmark :which-key "set snippet bookmark")
 "od" '(bmkp-dired-jump :which-key "jump to dired bookmark")

 "p" '(:ignore t :which-key "project")
 "pp" '(projectile-switch-project :which-key "projectile-switch-project")
 "pt" '(+ivy/tasks :which-key "+ivy/tasks")
 "pf" '(counsel-projectile-find-file :which-key "counsel-projectile-find-file")
 "px" '(projectile-invalidate-cache :which-key "projectile-invalidate-cache")
 "po" '(+term/open-popup-in-project :which-key "+term/open-popup-in-project")

 "w"  '(:ignore t :which-key "Windows")
 "wd" '(delete-window :which-key "delete window")
 "wD" '(ace-delete-window :which-key "ace delete window")
 "wF" '(make-frame :which-key "make frame")
 "w-" '(evil-window-split :which-key "split horizontally")
 "wv" '(evil-window-vsplit :which-key "split vertically")
 "wm" '(delete-other-windows :which-key "maximize window")
 "wt" '(window-split-toggle :which-key "toggle window layout")
 "ww" '(ace-window :which-key "ace window")
 "w TAB" '(aw-flip-window :which-key "select previous window")
 "wh" '(hydra-window/body :which-key "Window Hydra")
 "ws" '(ace-swap-window :which-key "swap window")
 "w=" '(balance-windows :which-key "balance windows")
 )

(general-omap
  :prefix "SPC"
  "." 'evil-avy-goto-char-2
  "l" 'evil-avy-goto-line
  "e" 'evil-avy-goto-subword-0 )

(general-omap
  "s"  'evil-surround-edit
  "S"  'evil-Surround-edit
  )

(general-vmap
  "S"  'evil-surround-region
  )

(general-define-key
 :states '(normal visual insert emacs)
 "C-y" '(yank :which-key "yank")
 "C-s" '(counsel-grep-or-swiper :which-key "swiper")
 "M-y" '(counsel-yank-pop :which-key "counsel yank pop")

 "C-h" '(evil-window-left :which-key "left window")
 "C-j" '(evil-window-down :which-key "down window")
 "C-k" '(evil-window-up :which-key "up window")
 "C-l" '(evil-window-right :which-key "right window")
 "M-/" '(dabbrev-expand :which-key "hippie expand")
 "C-c <left>" '(winner-undo :which-key "winner undo")
 "C-c <right>" '(winner-redo :which-key "winner redo")

 "<f9> r" '(rename-buffer :which-key "rename-buffer")
)

(general-define-key
 :states '(normal visual)
 "gc" '(evil-commentary :which-key "evil commentary")
 "gx" '(evil-exchange :which-key "evil exchange")
 "gp" '(+evil/reselect-paste :which-key "+evil/reselect-paste")
 "gr" '(+eval:region :which-key "+eval:region")
 "gR" '(+eval/buffer :which-key "+eval/buffer")
 )

;; NOTE: need to use 'override to make M-y works in evil-ex-map
(general-define-key
 :keymaps 'override
 "M-y" '(counsel-yank-pop :which-key "counsel-yank-pop")
 )

(general-define-key
 :states '(normal)
 :keymaps 'helpful-mode-map
 "q" '(+workspace/close-window-or-workspace :which-key "close window")
 )

(evil-set-initial-state 'compilation-mode 'normal)
(general-define-key
 :states '(normal)
 :keymaps 'compilation-mode-map
 :prefix "SPC"
 "gg" '(evil-goto-first-line :which-key "go to begining")
 "gr" '(recompile :which-key "recompile")
 "go" '(compilation-display-error :which-key "display error")
 "ep" '(compilation-previous-error :which-key "previous error")
 "en" '(compilation-next-error :which-key "next error"))
;(toggle-frame-maximized)
