(defsystem :jpeg-turbo
    :description "libjpeg-turbo wrapper for Common Lisp"
    :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :licence "2-clause BSD"
    :version "0.0"
    :defsystem-depends-on (:cffi-grovel)
    :depends-on (:cffi)
    :serial t
    :components ((:file "src/package")
                 (:cffi-grovel-file "src/grovel")
                 (:file "src/jpeg-turbo")))
