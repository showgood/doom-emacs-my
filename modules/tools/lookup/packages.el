;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm))

;;
(package! dumb-jump :pin "b5185e3368")
(when (featurep! :completion ivy)
  (package! ivy-xref :pin "3d4c35fe2b"))
(when (featurep! :completion helm)
  (package! helm-xref :pin "6b4a8bd91f"))

(when (featurep! +docsets)
  (package! dash-docs :pin "111fd9b970")
  (when (featurep! :completion helm)
    (package! helm-dash :pin "7f853bd34d"))
  (when (featurep! :completion ivy)
    (package! counsel-dash :pin "370d5f6f14")))

(when (featurep! +dictionary)
    (package! sdcv :recipe (:host github :repo "manateelazycat/sdcv"))
    (package! powerthesaurus :pin "81a262ec0c")
    (package! request)
    (when (featurep! +offline)
      (package! synosaurus :pin "14d34fc92a")))
