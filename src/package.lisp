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
           #:compress-to-octets

           #:+flag-bottomup+
           #:+flag-fast-upsample+
           #:+flag-norealloc+
           #:+flag-fast-dct+
           #:+flag-accurate-dct+
           #:+flag-stop-on-warning+
           #:+flag-progressive+))
