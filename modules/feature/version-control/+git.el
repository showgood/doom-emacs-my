;;; feature/version-control/+git.el -*- lexical-binding: t; -*-
;;;###if (not (featurep! -git))

(when (featurep! :feature evil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))


(def-package! gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")


(def-package! gitignore-mode
  :mode "/\\.gitignore$")


(def-package! git-gutter-fringe
  :commands git-gutter-mode
  :init
  (defun +version-control|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode +1)))
  (add-hook! (text-mode prog-mode conf-mode) #'+version-control|git-gutter-maybe)
  :config
  (set! :popup "^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (after! evil
    (defun +version-control|update-git-gutter ()
      "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`+evil-esc-hook' hooks."
      (when git-gutter-mode
        (ignore (git-gutter))))
    (add-hook '+evil-esc-hook #'+version-control|update-git-gutter t))

  (def-hydra! +version-control@git-gutter
    (:body-pre (git-gutter-mode 1) :hint nil)
    "
                                     ╭─────────────────┐
  Movement   Hunk Actions     Misc.  │ gg: +%-4s(car (git-gutter:statistic))/ -%-3s(cdr (git-gutter:statistic)) │
  ╭──────────────────────────────────┴─────────────────╯
     ^_g_^       [_s_] stage        [_R_] set start Rev
     ^_k_^       [_r_] revert
     ^↑ ^      [_m_] mark
     ^↓ ^      [_p_] popup          ╭──────────────────────
     ^_j_^                          │[_q_] quit
     ^_G_^                          │[_Q_] Quit and disable"
    ("j" (progn (git-gutter:next-hunk 1) (recenter)))
    ("k" (progn (git-gutter:previous-hunk 1) (recenter)))
    ("g" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
    ("G" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("m" git-gutter:mark-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (git-gutter-mode -1) :color blue)))


(def-package! git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)

  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details nil)
  (add-hook 'git-timemachine-mode-hook #'+vcs|init-header-line)
  (advice-add #'git-timemachine-show-revision :after #'+vcs*update-header-line)

  ;; Force evil to rehash keybindings for the current state
  (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state))


(def-package! magit
  :commands (magit-status magit-blame)
  :config
  (set! :evil-state 'magit-status-mode 'emacs)
  (after! evil
    ;; Switch to emacs state only while in `magit-blame-mode', then back when
    ;; its done (since it's a minor-mode).
    (add-hook! 'magit-blame-mode-hook
      (evil-local-mode (if magit-blame-mode -1 +1)))))


(def-package! git-link
  :commands (git-link git-link-commit git-link-homepage))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(defhydra hydra-git
  (:body-pre (git-gutter-mode 1)
   ;; :post (progn (kill-diff-buffers)
   ;;              (message "killed diff buffers"))
   :hint nil)
  "
^NAV^               ^HUNK^            ^FILES^        ^ACTIONS^
  _n_: next hunk    _s_tage hunk      _S_tage        _c_ommit
  _p_: prev hunk    _r_evert hunk     _R_evert       _b_lame
  _g_: first hunk   _P_opup hunk      _d_iff         _C_heckout
  _G_: last hunk    _R_evision start  _t_imemachine
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("g" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
  ("G" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
  ("j" my-goto-git-gutter)
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("P" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("S" magit-stage-file)
  ("R" magit-revert)
  ("d" magit-diff-unstaged :color blue)
  ("t" git-timemachine :color blue)
  ("c" magit-commit :color blue)
  ("b" magit-blame)
  ("C" magit-checkout)
  ("v" magit-status "status" :color blue)
  ("q" nil "quit" :color blue)
  ("Q" (progn
         (git-gutter-mode -1)
         ;; git-gutter-fringe doesn't seem to
         ;; clear the markup right away
         (sit-for 0.1)
         (git-gutter:clear))
   "quit git-gutter"
   :color blue))

(require 'evil-magit)
