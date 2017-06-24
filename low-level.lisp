#|
 This file is a part of cl-fond
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fond.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(define-foreign-library libfond
  (:darwin (:or "libfond.dylib" "libfond.so"
                #+X86 "mac32-libfond.dylib"
                #+X86-64 "mac64-libfond.dylib"))
  (:unix (:or "libfond.so"
              #+X86 "lin32-libfond.so"
              #+X86-64 "lin64-libfond.so"))
  (:windows (:or "out123.dll"
                 #+X86 "win32-libfond.dll"
                 #+X86-64 "win64-libfond.dll"))
  (t (:default "fond")))

(use-foreign-library libfond)

(defctype size_t :uint)

(defcenum error
  :no-error
  :file-load-failed
  :out-of-memory
  :font-pack-failed
  :font-init-failed
  :opengl-error
  :size-exceeded
  :not-loaded
  :utf8-conversion-error
  :unloaded-glyph
  :no-characters-or-codepoints
  :unknown)

(defcstruct (font :class font :conc-name font-)
  ;; Font info
  (file :pointer)
  (index :int)
  (size :float)
  (characters :pointer)
  (codepoints :pointer)
  ;; Buffer info
  (width :uint)
  (height :uint)
  (oversample :uint)
  (atlas :uint)
  ;; Internal data
  (fontdata :pointer)
  (chardata :pointer)
  (fontinfo :pointer)
  (converted-codepoints :int))

(defcstruct (buffer :class buffer :conc-name buffer-)
  (font :pointer)
  (texture :uint)
  (width :uint)
  (height :uint)
  (program :uint)
  (framebuffer :uint))

(defcstruct (extent :class extent :conc-name extent-)
  (l :float)
  (r :float)
  (t :float)
  (b :float)
  (gap :float))

(defcfun (free-font "fond_free") :void
  (font (:pointer (:struct font))))

(defcfun (load-font "fond_load") :boolean
  (font (:pointer (:struct font))))

(defcfun (load-font-fit "fond_load_fit") :boolean
  (font (:pointer (:struct font)))
  (max-size :uint))

(defcfun (compute-text "fond_compute") :boolean
  (font (:pointer (:struct font)))
  (text :string)
  (n (:pointer size_t))
  (vao (:pointer :uint)))

(defcfun (compute-text-u "fond_compute_u") :boolean
  (font (:pointer (:struct font)))
  (text :pointer)
  (size size_t)
  (n (:pointer size_t))
  (vao (:pointer :uint)))

(defcfun (update-text "fond_update") :boolean
  (font (:pointer (:struct font)))
  (text :string)
  (n (:pointer size_t))
  (vbo :uint)
  (ebo :uint))

(defcfun (update-text-u "fond_update_u") :boolean
  (font (:pointer (:struct font)))
  (text :pointer)
  (size size_t)
  (n (:pointer size_t))
  (vbo :uint)
  (ebo :uint))

(defcfun (compute-extent "fond_compute_extent") :boolean
  (font (:pointer (:struct font)))
  (text :string)
  (extent (:pointer extent)))

(defcfun (compute-extent-u "fond_compute_extent_u") :boolean
  (font (:pointer (:struct font)))
  (text :pointer)
  (size size_t)
  (extent (:pointer extent)))

(defcfun (free-buffer "fond_free_buffer") :void
  (buffer (:pointer (:struct buffer))))

(defcfun (load-buffer "fond_load_buffer") :boolean
  (buffer (:pointer (:struct buffer))))

(defcfun (render-buffer "fond_render_buffer") :boolean
  (buffer (:pointer (:struct buffer)))
  (text :string)
  (x :float)
  (y :float)
  (color (:pointer :float)))

(defcfun (render-buffer-u "fond_render_buffer_u") :boolean
  (buffer (:pointer (:struct buffer)))
  (text :pointer)
  (size size_t)
  (x :float)
  (y :float)
  (color (:pointer :float)))

(defcfun (decode-utf8 "fond_decode_utf8") :boolean
  (string :string)
  (decoded (:pointer :pointer))
  (size (:pointer size_t)))

(defcfun (fond-error "fond_error") error)

(defcfun (fond-error-string "fond_error_string") :string
  (error error))
