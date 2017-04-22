#|
 This file is a part of cl-fond
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fond)

(defun show-error ()
  (let ((error (cl-fond-cffi:fond-error)))
    (unless (eql error :no-error)
      (error "Fond error: ~a" (cl-fond-cffi:fond-error-string error)))))

(defclass c-object ()
  ((handle :initform NIL :initarg :handle :accessor handle)))

(defmethod allocate-handle (class))
(defmethod free-handle (class handle))

(defmethod initialize-instance :after ((c-object c-object) &key)
  (unless (handle c-object)
    (let ((handle (allocate-handle c-object)))
      (setf (handle c-object) handle)
      (tg:finalize c-object (free-handle c-object handle)))))

(defclass font (c-object)
  ())

(defun string->char* (string)
  (cffi:foreign-string-alloc string :encoding :utf-8))

(defmethod initialize-instance :after ((font font) &key file (index 0) (size 20) oversample charset)
  (let ((handle (handle font)))
    (setf (cl-fond-cffi:font-file handle)
          (string->char* (etypecase file
                           (string file)
                           (pathname (uiop:native-namestring file)))))
    (setf (cl-fond-cffi:font-index handle) index)
    (setf (cl-fond-cffi:font-size handle) (coerce size 'single-float))
    (setf (cl-fond-cffi:font-characters handle) (string->char* charset))
    (when oversample (setf (cl-fond-cffi:font-oversample handle) oversample))
    (unless (cl-fond-cffi:load-font-fit handle (cl-opengl:get* :max-texture-size))
      (show-error))))

(defun make-font (file charset &rest args &key index size oversample)
  (declare (ignore index size oversample))
  (apply #'make-instance 'font :file file :charset charset args))

(defmethod allocate-handle ((font font))
  (cffi:foreign-alloc '(:struct cl-fond-cffi:font)))

(defmethod free-handle ((font font) handle)
  (lambda ()
    (cffi:foreign-string-free (cl-fond-cffi:font-file handle))
    (cffi:foreign-string-free (cl-fond-cffi:font-characters handle))
    (cl-fond-cffi:free-font handle)))

(defmethod compute-text ((font font) text)
  (with-foreign-objects ((n 'cl-fond-cffi:size_t)
                         (vao :uint))
    (unless (cl-fond-cffi:compute-text (handle font) text n vao)
      (show-error))
    (values (cffi:mem-ref vao :uint)
            (cffi:mem-ref n 'cl-fond-cffi:size_t))))

(defmethod compute-extent ((font font) text)
  (with-foreign-object (extent '(:struct cl-fond-cffi:extent))
    (unless (cl-fond-cffi:compute-extent (handle font) text extent)
      (show-error))
    (cffi:mem-ref extent '(:struct cl-fond-cffi:extent))))

(defmethod file ((font font))
  (uiop:parse-native-namestring
   (cl-fond-cffi:font-file (handle font))))

(defmethod size ((font font))
  (cl-fond-cffi:font-size (handle font)))

(defmethod height ((font font))
  (with-foreign-object (extent '(:struct cl-fond-cffi:extent))
    (unless (cl-fond-cffi:compute-extent-u (handle font) 0 0 extent)
      (show-error))
    (cl-fond-cffi:extent-t extent)))

(defmethod atlas-width ((font font))
  (cl-fond-cffi:font-width (handle font)))

(defmethod atlas-height ((font font))
  (cl-fond-cffi:font-height (handle font)))

(defmethod atlas ((font font))
  (cl-fond-cffi:font-atlas (handle font)))

(defmethod charset ((font font))
  (cl-fond-cffi:font-characters (handle font)))

(defclass buffer (c-object)
  ((font :initarg :font :accessor font)))

(defmethod initialize-instance :after ((buffer buffer) &key font width height)
  (let ((handle (handle font)))
    (check-type font font)
    (setf (cl-fond-cffi:buffer-font handle) (handle font))
    (setf (cl-fond-cffi:buffer-width handle) width)
    (setf (cl-fond-cffi:buffer-height handle) height)
    (unless (cl-fond-cffi:load-buffer handle)
      (show-error))))

(defun make-buffer (font width height)
  (make-instance 'buffer :font font :width width :height height))

(defmethod allocate-handle ((buffer buffer))
  (cffi:foreign-alloc '(:struct cl-fond-cffi:buffer)))

(defmethod free-handle ((buffer buffer) handle)
  (lambda () (cl-fond-cffi:free-buffer handle)))

(defmethod render ((buffer buffer) text &key (x 0) (y 0) color)
  (with-foreign-object (_color :float 4)
    (destructuring-bind (&optional (r 1.0) (g 1.0) (b 1.0) (a 1.0)) color
      (setf (cffi:mem-ref _color :float 0) r)
      (setf (cffi:mem-ref _color :float 1) g)
      (setf (cffi:mem-ref _color :float 2) b)
      (setf (cffi:mem-ref _color :float 3) a))
    (unless (cl-fond-cffi:render-buffer (handle buffer) text x y _color)
      (show-error))
    (cl-fond-cffi:buffer-texture (handle buffer))))

(defmethod width ((buffer buffer))
  (cl-fond-cffi:buffer-width (handle buffer)))

(defmethod height ((buffer buffer))
  (cl-fond-cffi:buffer-height (handle buffer)))

(defmethod texture ((buffer buffer))
  (cl-fond-cffi:buffer-texture (handle buffer)))
