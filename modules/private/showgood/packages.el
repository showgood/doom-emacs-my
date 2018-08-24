;; -*- no-byte-compile: t; -*-

(package! vlf)
(package! engine-mode)
(package! eacl)
(package! beacon)
(package! fancy-narrow)
(package! dash-at-point)
(package! json-snatcher)
(package! hl-anything)
(package! origami)
(package! move-text)
(package! deft)
(package! suggest)
(package! counsel-etags)
(package! counsel-dash)
(package! yankpad)
(package! tldr)
(package! visual-regexp)
(package! visual-regexp-steroids)
(package! atomic-chrome)
(package! langtool)
(package! elpa-mirror)
(package! lsp-mode)
(package! ccls)
(package! company-lsp)

;; not very useful, buffer disappeared after window close and can't get back
;; (package! scratch-el :recipe (:fetcher github :repo "ieure/scratch-el"))
;; seems have some issue with evil mode
;; (package! json-navigator :recipe (:fetcher github :repo "DamienCassou/json-navigator"))

;; not quite working for me, ob-ipython seems better
;; https://github.com/millejoh/emacs-ipython-notebook
;; (package! ein)

;; couldn't get it installed on emacs 26
;; (package! org-preview-html)
;; (package! org-preview-html :recipe (:fetcher github :repo "lujun9972/org-preview-html"))

;; couldn't get it working properly on mac
;; (package! camcorder)

;; elpy seems better suits me
;; https://github.com/proofit404/anaconda-mode

;; not very useful
;; (package! logview)

;; no longer using
;; (package! paperless)

;; not working for me
;; (package! esup)
;; (package! ox-clip)
;; (package! ox-ipynb)
;;(package! org-ehtml)

;; to be explored
;; (package! wand)
;; (package! lentic)
;; (package! nov)
(package! aggressive-indent)
