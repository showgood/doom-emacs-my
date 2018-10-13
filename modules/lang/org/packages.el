;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org-plus-contrib)
(package! org-bullets)
(package! toc-org)
(package! evil-org)

(when (featurep! +attach)
  (package! org-download)
  (package! org-attach-screenshot)
)

(when (featurep! +babel)
  (package! ob-go)
  ;(package! ob-mongo)
  ;(package! ob-redis)
  (package! ob-restclient)
  ;(package! ob-rust)
  (package! ob-sql-mode)
  (package! ob-ipython))
  ;(package! ob-translate))

(when (featurep! +export)
  (package! ox-pandoc)
  (package! ox-gfm)
)

(when (featurep! +present)
  (package! centered-window-mode)
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))
