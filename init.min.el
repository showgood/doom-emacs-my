;;; init.el -*- lexical-binding: t; -*-
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;;
;;   =================     ===============     ===============   ========  ========
;;   \\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
;;   ||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
;;   || . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
;;   ||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
;;   || . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
;;   ||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
;;   || . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
;;   ||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
;;   ||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
;;   ||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
;;   ||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
;;   ||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
;;   ||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
;;   ||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
;;   ||.=='    _-'                                                     `' |  /==.||
;;   =='    _-'                                                            \/   `==
;;   \   _-'                                                                `-_   /
;;    `''                                                                      ``'
;;
;; These demons are not part of GNU Emacs.
;;
;;; License: MIT

(require 'core (concat user-emacs-directory "core/core"))

(defvar doom-mode "minimal")

(doom! :feature
       eval                             ; run code, run (also, repls)
       evil                             ; come to the dark side, we have cookies
       services               ; TODO managing external services & code builders
       snippets               ; my elves. They type so I don't have to
       version-control        ; remember, remember that commit in November
       workspaces             ; tab emulation, persistence & separate workspaces

       :completion
       company  ; the ultimate code completion backend
       ivy      ; a search engine for love and life

       :ui
       doom                      ; what makes DOOM look the way it does
       doom-dashboard            ; a nifty splash screen for Emacs
       doom-modeline             ; a snazzy Atom-inspired mode-line
       doom-quit                 ; DOOM quit-message prompts when you quit Emacs
       hl-todo                   ; highlight TODO/FIXME/NOTE tags
       (window-select +ace-window)      ; visually switch windows

       :tools
       dired                        ; making dired pretty [functional]
       imenu                        ; an imenu sidebar and searchable code index
       password-store                   ; password manager for nerds
       term                      ; terminals in Emacs

       :lang
       cc                               ; C/C++/Obj-C madness
       emacs-lisp                       ; drown in parentheses
       javascript                ; all(hope(abandon(ye(who(enter(here))))))
       lua                       ; one-based indices? one-based indices
       markdown                  ; writing docs for people to ignore
       python                           ; beautiful is better than ugly

       ;; Private modules are where you place your personal configuration files.
       ;; By default, they are not tracked. There is one module included here,
       ;; the defaults module. It contains a Spacemacs-inspired keybinding
       ;; scheme and additional ex commands for evil-mode. Use it as a reference
       ;; for your own.
       ;; :private showgood
       )
