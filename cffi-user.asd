(asdf:defsystem #:cffi-user
  :serial t
  :depends-on (#:cl-ppcre #:alexandria #:cffi)
  :components ((:file "package")
               (:file "cffi-user")))

