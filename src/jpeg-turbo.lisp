(in-package :jpeg-turbo)

(define-condition jpeg-error (error)
  ((error-string :initarg :error-string
                 :reader jpeg-error-string))
  (:report (lambda (c s)
             (format s "Turbo-JPEG error: ~a"
                     (jpeg-error-string c))))
  (:documentation "Errors returned by libjpeg-turbo"))

(defparameter *pixel-sizes*
  '((:rgb  . 3)
    (:bgr  . 3)
    (:rgbx . 4)
    (:bgrx . 4)
    (:xbgr . 4)
    (:gray . 1)
    (:rgba . 4)
    (:bgra . 4)
    (:abgr . 4)
    (:argb . 4)
    (:cmyk . 4))
  "Size of a pixel in bytes for each pixel format")

(defctype tj-handle :pointer
  "Handle for libjpeg-turbo compressor, decompressor or transformer")

(define-foreign-library jpeg-turbo
  (:unix (:or "libturbojpeg.so.0" "libturbojpeg.so"))
  (t (:default "libturbojpeg")))
(use-foreign-library jpeg-turbo)

(defun init-decompress% ()
  "Initialize decompressor handle or signal an error. The handle must
be destroyed after use with @c(destroy-handle%). It's preferred to use
@c(with-decompressor) macro which safely destroys the handle in the
case of an error instead of this function."
  (let ((handle (foreign-funcall "tjInitDecompress" tj-handle)))
    (if (null-pointer-p handle)
        (error 'jpeg-error
               :error-string "Cannot create a decompressor handle")
        handle)))

(defcfun ("tjDestroy" destroy-handle%) :void
  (handle tj-handle))

(defcfun ("tjGetErrorStr2" last-error) :string
  (handle tj-handle))

(defmacro with-decompressor ((handle) &body body)
  "Execute the macro's body in the lexical scope of created
decompressor handle @c(handle). The handle is safely freed after use."
  `(let ((,handle (init-decompress%)))
     (unwind-protect
          (progn ,@body)
       (destroy-handle% ,handle))))

(defun decompress-header-from-octets (handle array)
  "Decompress header of an image. @c(handle) must be a handle to
  decompressor created with @c(with-decompressor). @c(array) must be a
  simple array of @c((unsigned-byte 8)) values containing compressed
  jpeg image. Function returns four values:
@begin(enum)
    @item(Width of an image.)
    @item(Height of an image.)
    @item(Chroma subsampling.)
    @item(Colorspace.)
@end(enum)"
  (declare (type (simple-array (unsigned-byte 8)) array))
  (with-foreign-objects ((width      :int 1)
                         (height     :int 1)
                         (subsamp    :int 1)
                         (colorspace :int 1))
    (with-pointer-to-vector-data (array-ptr array)
      (let ((code (foreign-funcall "tjDecompressHeader3"
                                   tj-handle handle
                                   :pointer array-ptr
                                   :ulong (length array)
                                   :pointer width
                                   :pointer height
                                   :pointer subsamp
                                   :pointer colorspace
                                   :int)))
        (if (zerop code)
            (values
             (mem-aref width  :int)
             (mem-aref height :int)
             (foreign-enum-keyword 'subsamp    (mem-aref subsamp :int))
             (foreign-enum-keyword 'colorspace (mem-aref colorspace :int)))
            (error 'jpeg-error :error-string (last-error handle)))))))

(defun decompress-header (handle filename)
  "This is function is like @c(decompress-header-from-octets) but
reads compressed image directly from file with the name @c(filename)."
  (with-open-file (input filename
                   :element-type '(unsigned-byte 8))
    (let ((array (make-array (file-length input)
                             :element-type '(unsigned-byte 8))))
      (read-sequence array input)
      (decompress-header-from-octets handle array))))

(defun pixel-size (pixel-format)
  (or
   (cdr (assoc pixel-format *pixel-sizes*))
   (error 'jpeg-error :error-string "Wrong pixel format")))

(defun decompress-from-octets (handle array
                               &key
                                 (width  0)
                                 (height 0)
                                 (pixel-format :rgb))
  "Decompress a jpeg image. @c(handle) is a decompressor handle
created with @c(with-decompressor). @c(array) is a simple array of
@c((unsigned-byte 8)) values containing a compressed image. If
@c(width), @c(height) or @c(pixel-format) is specified,
@c(libjpeg-turbo) converts the resulting image to these new
specifications.
Return a decompressed image as a simple-array of @c((unsigned-byte 8))
value in the same manner as @c(cl-jpeg) does."
  (declare (type (simple-array (unsigned-byte 8)) array)
           (type unsigned-byte width height))
  (multiple-value-bind (orig-width orig-height)
      (decompress-header handle array)
    (setq width  (if (zerop width)  orig-width  width)
          height (if (zerop height) orig-height height)))
  (let ((decoded-array
         (make-shareable-byte-vector
          (* width height (pixel-size pixel-format)))))
    (with-pointer-to-vector-data (array-ptr array)
      (with-pointer-to-vector-data (decoded-ptr decoded-array)
        (let ((code
               (foreign-funcall "tjDecompress2"
                                tj-handle handle
                                :pointer array-ptr
                                :ulong (length array)
                                :pointer decoded-ptr
                                :int width
                                :int 0
                                :int height
                                pixel-format pixel-format
                                :int +flag-norealloc+
                                :int)))
          (if (zerop code)
              decoded-array
              (error 'jpeg-error :error-string (last-error handle))))))))

(defun decompress (handle filename
                   &key
                     (width  0)
                     (height 0)
                     (pixel-format :rgb))
  "Decompress an image directly from file with the name
@c(filename). See @c(decompress-from-octets) for more info"
  (with-open-file (input
                   filename
                   :element-type '(unsigned-byte 8))
    (let ((array (make-array (file-length input)
                             :element-type '(unsigned-byte 8))))
      (read-sequence array input)
      (decompress-from-octets handle array
                              :width width
                              :height height
                              :pixel-format pixel-format))))

(defun init-compress% ()
  "Initialize jpeg compressor returning a handle to it. The handle
must be freed with @c(destroy-handle%) after use. Use
@c(with-compressor) macro instead of this function."
  (let ((handle (foreign-funcall "tjInitCompress" tj-handle)))
    (if (null-pointer-p handle)
        (error 'jpeg-error
               :error-string "Cannot create a compressor handle")
        handle)))

(defmacro with-compressor ((handle) &body body)
  "Execute the macro's body in the lexical scope of created
compressor handle @c(handle). The handle is safely freed after use."
  `(let ((,handle (init-compress%)))
     (unwind-protect
          (progn ,@body)
       (destroy-handle% ,handle))))

(defcfun ("tjBufSize" buf-size%) :ulong
  (width  :int)
  (height :int)
  (subsamp subsamp))

(defun compress-to-octets (handle array
                           width height pixel-format
                           &key
                             (quality 90)
                             (subsamp :s-444))
  "Compress an image to jpeg format. @c(handle) is a compressor handle
created with @c(with-compressor). @c(array) is a simple array of
@c((unsigned-byte 8)) values."
  (declare (type (simple-array (unsigned-byte 8)) array)
           (type unsigned-byte width height)
           (type (integer 1 100) quality))
  (let ((output (make-shareable-byte-vector (buf-size% width height subsamp))))
    (with-pointer-to-vector-data (output-ptr output)
      (with-pointer-to-vector-data (input-ptr array)
        (with-foreign-objects ((output-ptr-ptr :pointer 1)
                               (output-size    :ulong   1))
          (setf (mem-aref output-ptr-ptr :pointer) output-ptr
                (mem-aref output-size    :ulong)   (length output))
          (let ((code (foreign-funcall "tjCompress2"
                                       tj-handle handle
                                       :pointer input-ptr
                                       :int width
                                       :int 0
                                       :int height
                                       pixel-format pixel-format
                                       :pointer output-ptr-ptr
                                       :pointer output-size
                                       subsamp subsamp
                                       :int quality
                                       :int +flag-norealloc+
                                       :int)))
            (if (zerop code)
                (subseq output 0 (mem-aref output-size :ulong))
                (error 'jpeg-error :error-string (last-error handle)))))))))

(defun compress (handle filename array
                 width height pixel-format
                 &key
                   (quality 90)
                   (subsamp :s-444))
  "Compress an image to jpeg format and write it to file with the name
@c(filename). See @c(compress-to-octets) for more info."
  (with-open-file (output filename
                          :direction :output
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence
     (compress-to-octets handle array
                         width height pixel-format
                         :quality quality
                         :subsamp subsamp)
     output)))
