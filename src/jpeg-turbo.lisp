(in-package :jpeg-turbo)

;; Conditions
(define-condition jpeg-error (error)
  ((error-string :initarg :error-string
                 :type string
                 :reader jpeg-error-string))
  (:report (lambda (c s)
             (format s "Turbo-JPEG error: ~a"
                     (jpeg-error-string c))))
  (:documentation "Errors returned by libjpeg-turbo"))

;; Library
(define-foreign-library jpeg-turbo
  (:os-macosx (:or "libturbojpeg.0.dylib" "libturbojpeg.dylib"))
  (:unix (:or "libturbojpeg.so.0" "libturbojpeg.so"))
  (t (:default "libturbojpeg"))
  )
(use-foreign-library jpeg-turbo)

;; Pixel sizes
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

(defun pixel-size (pixel-format)
  (or
   (cdr (assoc pixel-format *pixel-sizes*))
   (error 'jpeg-error :error-string "Wrong pixel format")))


;; Handlers
(defctype tj-handle :pointer
  "Handle for libjpeg-turbo compressor, decompressor or transformer")

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

(defmacro with-decompressor ((handle) &body body)
  "Execute the macro's body in the lexical scope of created
decompressor handle @c(handle). The handle is safely freed after use."
  `(let ((,handle (init-decompress%)))
     (unwind-protect
          (progn ,@body)
       (destroy-handle% ,handle))))

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

;; Misc
(defcfun ("tjGetErrorStr2" last-error) :string
  (handle tj-handle))

(defcfun ("tjBufSize" buf-size%) :ulong
  (width  :int)
  (height :int)
  (subsamp subsamp))

(defcstruct scaling-factor
  (num   :int)
  (denom :int))

(defun scaling-factors ()
  "Return scaling factors supported by @c(libjpeg-turbo)"
  (with-foreign-object (num :int)
    (let ((scaling-factors
           (foreign-funcall "tjGetScalingFactors"
                            :pointer num
                            :pointer)))
      (loop
         for i below (mem-aref num :int)
         for scaling-factor = (mem-aref scaling-factors
                                        '(:struct scaling-factor)
                                        i)
         collect (/ (getf scaling-factor 'num)
                    (getf scaling-factor 'denom))))))

(defun scale (x scaling)
  (ceiling
   (* x scaling)))

;; Decompression
(defun decompress-header-from-octets (handle array)
  "Decompress header of an image. @c(handle) must be a handle to
  decompressor created with @c(with-decompressor). @c(array) must be a
  simple array of @c((unsigned-byte 8)) values containing compressed
  jpeg image. Function returns four values:
@begin(enum)
    @item(Width of an image.)
    @item(Height of an image.)
    @item(Chroma subsampling. See @ref[id=subsamp](subsampling) section.)
    @item(Colorspace. See @ref[id=cs](colorspaces) section.)
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

(defun decompress-from-octets (handle array
                               &key
                                 (scaling-factor 1)
                                 (pixel-format :rgb)
                                 flags)
  "Decompress a jpeg image. @c(handle) is a decompressor handle
created with @c(with-decompressor). @c(array) is a simple array of
@c((unsigned-byte 8)) values containing a compressed image. If
@c(pixel-format) is specified, @c(libjpeg-turbo) converts output pixel
format to a specified value. For the values of @c(pixel-format) see
the section @ref[id=pf](pixel formats). For more information about
@c(flags) see the section @ref[id=flags](flags). If @c(scaling-factor)
is specified @c(librurbo-jpeg) will scale the output image to this
scaling factor. Possible values are returned by @c(scaling-factors).

Return a decompressed image as a simple-array of @c((unsigned-byte 8))
value in the same manner as @c(cl-jpeg) does."
  (declare (type (simple-array (unsigned-byte 8)) array)
           (type rational scaling-factor))
  (multiple-value-bind (width height)
      (decompress-header-from-octets handle array)
    (setq width  (scale width  scaling-factor)
          height (scale height scaling-factor))
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
                                  ;; SBCL reports about dead code
                                  ;; here. This is OK, see
                                  ;; macroexpansion of
                                  ;; FOREIGN-FUNCALL (it is a macro)
                                  jpeg-turbo-flags (cons :no-realloc flags)
                                  :int)))
            (if (zerop code)
                (values
                 decoded-array
                 width height)
                (error 'jpeg-error :error-string (last-error handle)))))))))

(defun decompress (handle filename
                   &key
                     (scaling-factor 1)
                     (pixel-format :rgb)
                     flags)
  "Decompress an image directly from file with the name
@c(filename). See @c(decompress-from-octets) for more info"
  (with-open-file (input
                   filename
                   :element-type '(unsigned-byte 8))
    (let ((array (make-array (file-length input)
                             :element-type '(unsigned-byte 8))))
      (read-sequence array input)
      (decompress-from-octets handle array
                              :scaling-factor scaling-factor
                              :pixel-format pixel-format
                              :flags flags))))

;; Compression
(defun compress-to-octets (handle array
                           width height pixel-format
                           &key
                             (quality 90)
                             (subsamp :s-444)
                             flags)
  "Compress an image to jpeg format. @c(handle) is a compressor handle
created with @c(with-compressor). @c(array) is a simple array of
@c((unsigned-byte 8)) values containing pixel data. Values of
@c(pixel-format) described in section @ref[id=pf](pixel
formats). @c(quality) is an integer from 1 to 100. Higher values mean
better quality. @c(subsamp) is described in
@ref[id=subsamp](subsampling) section and @c(flags) in
@ref[id=flags](flags) section."
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
                                       ;; SBCL reports about dead code
                                       ;; here. This is OK, see
                                       ;; macroexpansion of
                                       ;; FOREIGN-FUNCALL (it is a macro)
                                       jpeg-turbo-flags (cons :no-realloc flags)
                                       :int)))
            (if (zerop code)
                (subseq output 0 (mem-aref output-size :ulong))
                (error 'jpeg-error :error-string (last-error handle)))))))))

(defun compress (handle filename array
                 width height pixel-format
                 &key
                   (quality 90)
                   (subsamp :s-444)
                   flags)
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
                         :subsamp subsamp
                         :flags flags)
     output)))
