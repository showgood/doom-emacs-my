;;; private/showgood/+commands.el -*- lexical-binding: t; -*-

(defalias 'ex! 'evil-ex-define-cmd)

;;; Commands defined elsewhere
;;(ex! "al[ign]"      #'+evil:align)
;;(ex! "g[lobal]"     #'+evil:global)

;;; Custom commands
;; Editing
(ex! "@"         #'+evil:macro-on-all-lines)   ; TODO Test me
(ex! "al[ign]"   #'+evil:align)
(ex! "enhtml"    #'+web:encode-html-entities)
(ex! "dehtml"    #'+web:decode-html-entities)
(ex! "mc"        #'+evil:mc)
(ex! "iedit"     #'evil-multiedit-ex-match)
(ex! "na[rrow]"  #'+evil:narrow-buffer)
(ex! "fna[rrow]" #'fancy-narrow-to-region)
(ex! "wi[den]"   #'widen)
(ex! "fwi[rrow]" #'fancy-widen)
(ex! "retab"     #'+evil:retab)
(ex! "a"         #'me/a)
(ex! "A"         #'me/A)

;; External resources
;; TODO (ex! "db"          #'doom:db)
;; TODO (ex! "dbu[se]"     #'doom:db-select)
;; TODO (ex! "go[ogle]"    #'doom:google-search)
(ex! "lo[okup]"    #'+jump:online)
(ex! "http"        #'httpd-start)            ; start http server
(ex! "repl"        #'+eval:repl)             ; invoke or send to repl
;; TODO (ex! "rx"          'doom:regex)             ; open re-builder
(ex! "sh[ell]"     #'+eshell:run)
(ex! "t[mux]"      #'+tmux:run)              ; send to tmux
(ex! "tcd"         #'+tmux:cd-here)          ; cd to default-directory in tmux
(ex! "x"           #'doom/open-project-scratch-buffer)

;; GIT
(ex! "gist"        #'+gist:send)  ; send current buffer/region to gist
(ex! "gistl"       #'+gist:list)  ; list gists by user
(ex! "gbrowse"     #'+vcs/git-browse)        ; show file in github/gitlab
(ex! "gissues"     #'+vcs/git-browse-issues) ; show github issues
(ex! "gst"         #'magit-status)           ; open magit status window
(ex! "gstage"      #'magit-stage)
(ex! "ga"          #'magit-stage)
(ex! "gd"          #'magit-diff-buffer-file)

;; show all the commits affect current file
(ex! "gl"          #'magit-log-buffer-file)

;; show all the commits affect a function
(ex! "gf"          #'magit-log-trace-definition)
(ex! "gunstage"    #'magit-unstage)
(ex! "gblame"      #'magit-blame)
(ex! "grevert"     #'git-gutter:revert-hunk)
(ex! "gp"          #'magit-push)

;; Dealing with buffers
(ex! "clean[up]"   #'doom/cleanup-buffers)
(ex! "k[ill]"      #'doom/kill-this-buffer)
(ex! "k[ill]all"   #'+hlissner:kill-all-buffers)
(ex! "k[ill]m"     #'+hlissner:kill-matching-buffers)
(ex! "k[ill]o"     #'doom/kill-other-buffers)
(ex! "l[ast]"      #'doom/popup-restore)
(ex! "m[sg]"       #'view-echo-area-messages)
(ex! "pop[up]"     #'doom/popup-this-buffer)

;; Project navigation
;; (ex! "a"           #'projectile-find-other-file)
(ex! "cd"       #'+hlissner:cd)
(ex! "ag"       #'+ivy:ag)
(ex! "agc[wd]"  #'+ivy:ag-cwd)
(ex! "rg"       #'+ivy:rg)
(ex! "rgc[wd]"  #'+ivy:rg-cwd)
(ex! "sw[iper]" #'+ivy:swiper)
(ex! "todo"     #'+ivy:todo)

;; Project tools
(ex! "build"       #'+eval/build)
(ex! "debug"       #'+debug/run)
(ex! "er[rors]"    #'flycheck-list-errors)

;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

;; Sessions/tabs
(ex! "sclear"      #'+workspace/kill-session)
(ex! "sl[oad]"     #'+workspace:load-session)
(ex! "ss[ave]"     #'+workspace:save-session)
(ex! "tabc[lose]"  #'+workspace:delete)
(ex! "tabclear"    #'doom/kill-all-buffers)
(ex! "tabl[ast]"   #'+workspace/switch-to-last)
(ex! "tabload"     #'+workspace:load)
(ex! "tabn[ew]"    #'+workspace:new)
(ex! "tabn[ext]"   #'+workspace:switch-next)
(ex! "tabp[rev]"   #'+workspace:switch-previous)
(ex! "tabr[ename]" #'+workspace:rename)
(ex! "tabs"        #'+workspace/display)
(ex! "tabsave"     #'+workspace:save)

;; Org-mode
(ex! "cap"         #'+org-capture/dwim)

;; clear the term-mode buffer
(ex! "cl"          #'comint-clear-buffer)
