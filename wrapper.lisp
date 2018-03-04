#|
 This file is a part of cl-fond
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.fond)

(define-condition fond-error (error)
  ((error-code :initarg :error-code :initform :unknown :accessor error-code))
  (:report (lambda (c s) (write-string (cl-fond-cffi:fond-error-string (error-code c)) s))))

(defun show-error ()
  (let ((code (cl-fond-cffi:fond-error)))
    (unless (eql code :no-error)
      (error 'fond-error :error-code code))))

(defun calloc (type)
  (let ((ptr (cffi:foreign-alloc type)))
    (dotimes (i (cffi:foreign-type-size type) ptr)
      (setf (cffi:mem-aref ptr :uchar i) 0))))

(defun string->char* (string)
  (cffi:foreign-string-alloc string :encoding :utf-8))

(defclass c-object ()
  ((handle :initform NIL :initarg :handle :accessor handle)))

(defgeneric allocate-handle (class))
(defgeneric free-handle (class handle))

(defmethod initialize-instance :after ((c-object c-object) &key)
  (unless (handle c-object)
    (let ((handle (allocate-handle c-object)))
      (setf (handle c-object) handle)
      (tg:finalize c-object (free-handle c-object handle)))))

(defmethod free ((object c-object))
  (let ((handle (handle object)))
    (when handle
      (tg:cancel-finalization object)
      (setf (handle object) NIL)
      (funcall (free-handle object handle)))))

(defclass font (c-object)
  ())

(defmethod initialize-instance :after ((font font) &key file (index 0) (size 20) oversample charset width height)
  (let ((handle (handle font)))
    (setf (cl-fond-cffi:font-file handle)
          (string->char* (etypecase file
                           (string file)
                           (pathname (uiop:native-namestring file)))))
    (setf (cl-fond-cffi:font-index handle) index)
    (setf (cl-fond-cffi:font-size handle) (coerce size 'single-float))
    (setf (cl-fond-cffi:font-characters handle) (string->char* charset))
    (when width (setf (cl-fond-cffi:font-width handle) width))
    (when height (setf (cl-fond-cffi:font-height handle) height))
    (when oversample (setf (cl-fond-cffi:font-oversample handle) oversample))
    (unless (cl-fond-cffi:load-font-fit handle (cl-opengl:get* :max-texture-size))
      (show-error))))

(defun make-font (file charset &rest args &key index size oversample width height)
  (declare (ignore index size oversample width heigt))
  (apply #'make-instance 'font :file file :charset charset args))

(defmethod allocate-handle ((font font))
  (calloc '(:struct cl-fond-cffi:font)))

(defmethod free-handle ((font font) handle)
  (lambda ()
    (cffi:foreign-string-free (cl-fond-cffi:font-file handle))
    (cffi:foreign-string-free (cl-fond-cffi:font-characters handle))
    (cl-fond-cffi:free-font handle)
    (cffi:foreign-free handle)))

(defun compute-text (font text)
  (with-foreign-objects ((n 'cl-fond-cffi:size_t)
                         (vao :uint))
    (unless (etypecase text
              #+sb-unicode
              ((not base-string)
               (cffi:with-pointer-to-vector-data (pointer text)
                 (cl-fond-cffi:compute-text-u (handle font) pointer (length text) n vao)))
              (string
               (cl-fond-cffi:compute-text (handle font) text n vao))))
    (values (cffi:mem-ref vao :uint)
            (cffi:mem-ref n 'cl-fond-cffi:size_t))))

(defun update-text (font text vbo ebo)
  (with-foreign-objects ((n 'cl-fond-cffi:size_t))
    (unless (etypecase text
              #+sb-unicode
              ((not base-string)
               (cffi:with-pointer-to-vector-data (pointer text)
                 (cl-fond-cffi:update-text-u (handle font) pointer (length text) n vbo ebo)))
              (string
               (cffi:with-foreign-string (pointer text :encoding :utf-32)
                 (cl-fond-cffi:update-text-u (handle font) pointer (length text) n vbo ebo))))
            (show-error))
    (cffi:mem-ref n 'cl-fond-cffi:size_t)))

(defun compute-extent (font text)
  (with-foreign-object (extent '(:struct cl-fond-cffi:extent))
    (unless (etypecase text
              #+sb-unicode
              ((not base-string)
               (cffi:with-pointer-to-vector-data (pointer text)
                 (cl-fond-cffi:compute-extent-u (handle font) pointer (length text) extent)))
              (string
               (cffi:with-foreign-string (pointer text :encoding :utf-32)
                 (cl-fond-cffi:compute-extent-u (handle font) pointer (length text) extent)))))
    (cffi:mem-ref extent '(:struct cl-fond-cffi:extent))))

(defun file (font)
  (uiop:parse-native-namestring
   (cl-fond-cffi:font-file (handle font))))

(defun size (font)
  (cl-fond-cffi:font-size (handle font)))

(defun text-height (font)
  (with-foreign-object (extent '(:struct cl-fond-cffi:extent))
    (unless (cl-fond-cffi:compute-extent-u (handle font) (cffi:null-pointer) 0 extent)
      (show-error))
    (cl-fond-cffi:extent-t extent)))

(defmethod width ((font font))
  (cl-fond-cffi:font-width (handle font)))

(defmethod height ((font font))
  (cl-fond-cffi:font-height (handle font)))

(defmethod texture ((font font))
  (cl-fond-cffi:font-atlas (handle font)))

(defun charset (font)
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
  (calloc '(:struct cl-fond-cffi:buffer)))

(defmethod free-handle ((buffer buffer) handle)
  (lambda ()
    (cl-fond-cffi:free-buffer handle)
    (cffi:foreign-free handle)))

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

(defmethod (setf font) :before (font (buffer buffer))
  (check-type font font)
  (setf (cl-fond-cffi:buffer-font (handle buffer)) (handle font)))
