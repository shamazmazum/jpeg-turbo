(defpackage jpeg-turbo
  (:use #:cl #:cffi)
  (:export #:jpeg-error
           #:with-decompressor
           #:with-compressor
           #:decompress-header
           #:decompress-header-from-octets
           #:decompress
           #:decompress-from-octets
           #:compress
           #:compress-to-octets))
