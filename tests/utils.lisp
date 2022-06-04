(in-package :we.test.utils)

(defparameter *shader-path* (concatenate 'string
					 (namestring (asdf:system-relative-pathname :wild-engine "tests"))
					 "/shaders"))

(defun build-shader-path (module file-name)
  (concatenate 'string
	       *shader-path*
	       "/"
	       module
	       "/"
	       file-name
	       ".spv"))

(defparameter *triangle-frag* (build-shader-path "triangle" "frag"))
(defparameter *triangle-vert* (build-shader-path "triangle" "vert"))

(defparameter *vertex-frag* (build-shader-path "vertex" "frag"))
(defparameter *vertex-vert* (build-shader-path "vertex" "vert"))

(defparameter *uniform-frag* (build-shader-path "uniform" "frag"))
(defparameter *uniform-vert* (build-shader-path "uniform" "vert"))


;;(defparameter *triangle-vert*)

