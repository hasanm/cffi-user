(in-package #:cffi-user)

(define-foreign-library libcurl
  (t (:default "libcurl")))

(use-foreign-library libcurl)

(defparameter *curl-error-size* 257)

(defcfun "curl_easy_init" :pointer)

(defctype curl-code :int)

(defclass easy-handle ()
  ((pointer :initform (curl-easy-init))
   (error-buffer :initform (foreign-alloc :char :count *curl-error-size*
                                                :initial-element 0))
   (c-strings :initform '())
   ))

(define-foreign-type easy-handle-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser easy-handle))

(defmethod translate-to-foreign (handle (type easy-handle-type))
  (slot-value handle 'pointer))

(defctype size :unsigned-int)

(defcfun "curl_global_init" curl-code
  (flags :long))

(defcfun "curl_easy_cleanup" :void
  (easy-handle :pointer))

(defcfun "curl_easy_perform" curl-code
  (handle easy-handle))

(defmacro define-curl-options (type-name type-offsets &rest enum-args)
  "DOCSTRING"
  (flet ((enumerated-value (type offset)
           (+ (getf type-offsets type) offset))
         (map-enum-args (procedure)
           (mapcar (lambda (arg) (apply procedure arg)) enum-args))
         (make-setter-name (option-name)
           (intern (concatenate
                    'string "SET-" (symbol-name type-name)
                    "-" (symbol-name option-name)))))

    `(progn
       (defcenum ,type-name
         ,@ (map-enum-args
             (lambda (name type number)
               (list name (enumerated-value type number)))))
       ,@ (map-enum-args
           (lambda (name type number)
             (declare (ignore number))
             `(define-curl-option-setter ,(make-setter-name name)
                ,type-name ,name ,(ecase type
                                    (long :long)
                                    (objectpoint :pointer)
                                    (functionpoint :pointer)
                                    (off-t :long)))))
       ',type-name)))


(defmacro curl-easy-setopt (easy-handle enumerated-name
                            value-type new-value)
  "DOCSTRING"
  `(foreign-funcall "curl_easy_setopt" easy-handle ,easy-handle
                    curl-option ,enumerated-name
                    ,value-type ,new-value curl-code))


(defun curry-curl-option-setter (function-name option-keyword)
  "Docstring"
  (setf (symbol-function function-name)
        (let ((c-function (symbol-function function-name)))
          (lambda (easy-handle new-value)
            (funcall c-function easy-handle option-keyword
                     (if (stringp new-value)
                         (add-curl-handle-cstring
                          easy-handle
                          (foreign-string-alloc new-value))
                         new-value))))))


(defmacro define-curl-option-setter (name option-type
                                     option-value foreign-type)
  "DocString"
  `(progn
     (defcfun ("curl_easy_setopt" ,name) curl-code
       (easy-handle easy-handle)
       (option ,option-type)
       (new-value ,foreign-type))
     (curry-curl-option-setter ',name ',option-value)))



(define-curl-options curl-option
    (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
  (:noprogress long 43 )
  (:nosignal long 99)
  (:errorbuffer objectpoint 10)
  (:url objectpoint 2)
  (:writefunction functionpoint 11))


(defcallback easy-write size ((ptr :pointer)
                              (size size)
                              (nmemb size)
                              (stream :pointer))
  (let ((data-size (* size nmemb)))
    (handler-case
        (progn (funcall (symbol-value '*easy-write-procedure*)
                        (foreign-string-to-lisp ptr :count data-size))
               data-size)
      (error () (if (zerop data-size) 1 0)))))

(define-foreign-type curl-code-type ()
  ()
  (:actual-type :int)
  (:simple-parser curl-code))


(define-condition curl-code-error (error)
  (($code :initarg :curl-code :reader curl-error-code))
  (:report (lambda (c stream ())
             (format stream "libcurl function returned error ~a"
                     (curl-error-code c))))
  (:documentation "Signalled when a libcurl function answers a code othern than CURLE_OK."))

(defmethod translate-from-foreign (value (type curl-code-type))
  "Raise a CURL-CODE-ERROR if VALUE, a curl-code, is non-zero."
  (if (zerop value)
      :curle-ok
      (error 'curl-code-error :curl-code value)))


(defmethod initialize-instance :after ((self easy-handle) &key)
  (set-curl-option-errorbuffer self (slot-value self 'error-buffer)))

(defun add-curl-handle-cstring (handle cstring )
  (car (push cstring (slot-value handle 'c-strings))))

(defun get-easy-handle-error (handle)
  (foreign-string-to-lisp
   (slot-value handle 'error-buffer)))

(defun free-easy-handle (handle)
  (with-slots (pointer error-buffer c-strings) handle
    (curl-easy-cleanup pointer)
    (foreign-free error-buffer)
    (mapc #'foreign-string-free c-strings)))

(defun get-url (url)
  (let ((handle (make-instance 'easy-handle)))
    (with-slots (pointer error-buffer c-strings ) handle
      (set-curl-option-nosignal handle 1)
      (set-curl-option-url handle url)
      (set-curl-option-writefunction handle (callback easy-write))
      (with-output-to-string (contents)
        (let ((*easy-write-procedure*
                (lambda (string)
                  (format t "TYPE : ~a~%" (type-of string))
                  (write-string string contents))))
          (declare (special *easy-write-procedure*))
          (curl-easy-perform handle)
          (format t "OUTPUT : ~a~%" (get-output-stream-string contents))))
      )
    (free-easy-handle handle)))
