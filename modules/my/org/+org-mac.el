(require 'org-mac-link)

(org-link-set-parameters
 "marginnote3app"
 :export (lambda (path desc backend)
    (cl-case backend
      (md (format "[%s](%s)" (or desc "")
                  (concat  "marginnote3app:" path))))))

(org-link-set-parameters
 "x-devonthink-item"
 :export (lambda (path desc backend)
    (cl-case backend
      (md (format "[%s](%s)" (or desc "")
                  (concat  "x-devonthink-item:" path))))))
