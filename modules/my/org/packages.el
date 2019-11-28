;; -*- no-byte-compile: t; -*-
;;; tools/myorg/packages.el

(package! org-attach-screenshot)
(package! ox-gfm)
(package! org-noter)
(package! jupyter)

;; depends on  https://github.com/francoislaberge/diagrams to generate diagrams
(package! ob-diagrams :recipe (:host github :repo "showgood/ob-diagrams"))
