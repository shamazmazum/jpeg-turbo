(in-package :jpeg-turbo)
(include "turbojpeg.h")
#+freebsd
(cc-flags "-I/usr/local/include")

;; Subsampling modes
(cenum subsamp
       ((:s-444  "TJSAMP_444"))
       ((:s-422  "TJSAMP_422"))
       ((:s-420  "TJSAMP_420"))
       ((:s-gray "TJSAMP_GRAY"))
       ((:s-440  "TJSAMP_440"))
       ((:s-411  "TJSAMP_411")))

;; Pixel formats
(cenum pixel-format
       ((:RGB  "TJPF_RGB"))
       ((:BGR  "TJPF_BGR"))
       ((:RGBX "TJPF_RGBX"))
       ((:BGRX "TJPF_BGRX"))
       ((:XBGR "TJPF_XBGR"))
       ((:gray "TJPF_GRAY"))
       ((:RGBA "TJPF_RGBA"))
       ((:BGRA "TJPF_BGRA"))
       ((:ABGR "TJPF_ABGR"))
       ((:ARGB "TJPF_ARGB"))
       ((:CMYK "TJPF_CMYK")))

;; Color spaces
(cenum colorspace
       ((:RGB   "TJCS_RGB"))
       ((:YCbCr "TJCS_YCbCr"))
       ((:gray  "TJCS_GRAY"))
       ((:CMYK  "TJCS_CMYK"))
       ((:YCCK  "TJCS_YCCK")))

;; Flags
(constant (+flag-bottomup+      "TJFLAG_BOTTOMUP"))
(constant (+flag-fast-upsample+ "TJFLAG_FASTUPSAMPLE"))
(constant (+flag-norealloc+     "TJFLAG_NOREALLOC"))
(constant (+flag-fast-dct+      "TJFLAG_FASTDCT"))
(constant (+flag-accurate-dct   "TJFLAG_ACCURATEDCT"))
(constant (+flag-stop-on-warning+ "TJFLAG_STOPONWARNING"))
(constant (+flag-progressive+   "TJFLAG_PROGRESSIVE"))
