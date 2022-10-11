(in-package #:test.util)

(defun make-shader-path (module-name &optional (shader-type :vert))
  (concatenate 'string
	       (namestring (asdf:system-relative-pathname :wild-engine "test/"))
	       module-name
	       "/shader/"
	       module-name
	       (case shader-type
		 (:vert "-vert.spv")
		 (:frag "-frag.spv")
		 (t (error "unknow shader type")))))
