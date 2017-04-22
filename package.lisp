#|
 This file is a part of cl-fond
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-fond-cffi
  (:nicknames #:org.shirakumo.fraf.fond.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:shadow :open :close :continue)
  (:export
   #:*static*
   #:libfond
   #:size_t
   #:error
   #:font
   #:font-file
   #:font-index
   #:font-size
   #:font-characters
   #:font-codepoints
   #:font-width
   #:font-height
   #:font-oversample
   #:font-atlas
   #:font-chardata
   #:font-fontinfo
   #:font-converted-codepoints
   #:buffer
   #:buffer-font
   #:buffer-texture
   #:buffer-width
   #:buffer-height
   #:buffer-program
   #:buffer-framebuffer
   #:extent
   #:extent-l
   #:extent-r
   #:extent-t
   #:extent-b
   #:extent-gap
   #:free-font
   #:load-font
   #:load-font-fit
   #:compute-text
   #:compute-text-u
   #:compute-extent
   #:compute-extent-u
   #:free-buffer
   #:load-buffer
   #:render-buffer
   #:render-buffer-u
   #:decode-utf8
   #:fond-error
   #:fond-error-string))

(defpackage #:cl-fond
  (:nicknames #:org.shirakumo.fraf.fond)
  (:use #:cl #:cffi)
  ;; wrapper.lisp
  (:export
   #:c-object
   #:handle
   #:font
   #:make-font
   #:compute-text
   #:compute-extent
   #:file
   #:size
   #:atlas-width
   #:atlas-height
   #:height
   #:atlas
   #:charset
   #:buffer
   #:make-buffer
   #:render
   #:texture
   #:width
   #:height))
