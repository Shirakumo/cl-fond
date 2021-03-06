#|
 This file is a part of cl-fond
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-fond
  :version "1.1.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to libfond, a simple text rendering engine for OpenGL"
  :homepage "https://Shirakumo.github.io/cl-fond/"
  :bug-tracker "https://github.com/Shirakumo/cl-fond/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-fond.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:alexandria
               :cffi
               :trivial-features
               :trivial-garbage
               :documentation-utils
               :cl-opengl))
