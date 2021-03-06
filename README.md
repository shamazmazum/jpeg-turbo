jpeg-turbo
==========
[![Build Status](https://api.cirrus-ci.com/github/shamazmazum/jpeg-turbo.svg)](https://cirrus-ci.com/github/shamazmazum/jpeg-turbo)
![CI](https://github.com/shamazmazum/jpeg-turbo/workflows/CI/badge.svg)

**jpeg-turbo** is a Common Lisp wrapper for `libjpeg-turbo` library
which provides TurboJPEG API for compressing and decompressing JPEG
images. To build this wrapper make sure that both `libjpeg-turbo`
library and headers are installed (on FreeBSD this is done by
installing `graphics/libjpeg-turbo` port).

## Examples

Read header of an image (get width, height, subsampling mode and
colorspace):
~~~~~~~~{.lisp}
(with-decompressor (handle)
    (decompress-header handle "example.jpg"))
~~~~~~~~

Decode a whole image, converting it to grayscale:
~~~~~~~~{.lisp}
(with-decompressor (handle)
    (decompress handle "example.jpg"
                :pixel-format :gray))
~~~~~~~~

Encode an image contained in `array`. Each pixel is encoded in three
elements of the array. These elements must be red, green and blue
components of the pixel.
~~~~~~~~{.lisp}
(with-compressor (handle)
    (compress handle "example.jpg" array
              width height :rgb))
~~~~~~~~

## Documentation
Visit the [project page](http://shamazmazum.github.io/jpeg-turbo). If
you want a local copy, run
`(codex:document :jpeg-turbo :skip-undocumented t)`.

## TODO
Add transformation API
