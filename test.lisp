(require :asdf)
(push #P"/home/p-hasan/src/lisp/cffi-user/" asdf:*central-registry*)
;; (require "cffi-user")
(ql:quickload "cffi-user")

;; (in-package #:cffi-user)

;; (cffi-user::curl-global-init 0)

; (cffi-user::get-url "http://www.sbcl.org/index.html")

(let ((handle (make-instance 'cffi-user::easy-handle)))
  (cffi-user::get-url "http://www.sbcl.org/index.html")
  )

(sb-ext:exit)
