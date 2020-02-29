(defsystem :jpeg-turbo
  :name :jpeg-turbo
  :description "libjpeg-turbo wrapper for Common Lisp"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :version "1.0"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi)
  :serial t
  :components ((:file "src/package")
               (:cffi-grovel-file "src/grovel")
               (:file "src/jpeg-turbo"))
  :in-order-to ((test-op (load-op "jpeg-turbo/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :jpeg-turbo-tests))))))

(defsystem :jpeg-turbo/tests
  :name :jpeg-turbo/tests
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :version "1.0"
  :components ((:file "tests/package")
               (:file "tests/tests" :depends-on ("tests/package")))
  :depends-on (:jpeg-turbo :fiveam))
