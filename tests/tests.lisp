(in-package :jpeg-turbo-tests)

(def-suite jpeg-turbo :description "Test jpeg-turbo")
(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(jpeg-turbo))))

(in-suite jpeg-turbo)
(defconstant +width+ 800)
(defconstant +height+ 600)
(defun compress (subsamp bps pf)
  (let ((array (cffi:make-shareable-byte-vector (* bps +width+ +height+))))
    (fill array 0)
    (jpeg-turbo:with-compressor (handle)
      (jpeg-turbo:compress-to-octets
       handle array
       +width+ +height+ pf
       :subsamp subsamp))))

(defun decompress (array)
  (jpeg-turbo:with-decompressor (handle)
    (jpeg-turbo:decompress-from-octets handle array)))

(test compression-decompression
  (mapc
   (lambda (subsamp bps pf)
     (finishes
       (decompress
        (compress subsamp bps pf))))
   '(:s-444 :s-422 :s-420 :s-gray :s-440 :s-411)
   '(3 3 3 1 3 3)
   '(:rgb :rgb :rgb :gray :rgb :rgb)))

(test read-headers-gray
  (let ((compressed (compress :s-gray 1 :gray)))
    (jpeg-turbo:with-decompressor (handle)
      (multiple-value-bind (width height subsamp colorspace)
          (jpeg-turbo:decompress-header-from-octets handle compressed)
        (is (= width +width+))
        (is (= height +height+))
        (is (eq subsamp :s-gray))
        (is (eq colorspace :gray))))))

(test read-headers-rgb
  (let ((compressed (compress :s-422 3 :rgb)))
    (jpeg-turbo:with-decompressor (handle)
      (multiple-value-bind (width height subsamp colorspace)
          (jpeg-turbo:decompress-header-from-octets handle compressed)
        (is (= width +width+))
        (is (= height +height+))
        (is (eq subsamp :s-422))
        (is (eq colorspace :ycbcr))))))
