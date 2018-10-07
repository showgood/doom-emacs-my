;; -*- no-byte-compile: t; -*-

(package! vlf)
(package! engine-mode)
(package! eacl)
(package! beacon)
(package! fancy-narrow)
(package! hl-anything)
(package! origami)
(package! move-text)
(package! atomic-chrome)
(package! elpa-mirror)
(package! ccls)
(package! deadgrep)


;; disable it until this issue is solved
;; https://github.com/Kungsgeten/yankpad/issues/24
;; (package! yankpad)

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

;; often it messes the existing code
;; (package! aggressive-indent)

;; not working for me
;; (package! esup)
;; (package! ox-clip)
;; (package! ox-ipynb)
;;(package! org-ehtml)

;; to be explored
;; (package! wand)
;; (package! lentic)
;; (package! nov)
