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

(defun make-texture-path (module-name texture-name)
  (concatenate 'string
	       (namestring (asdf:system-relative-pathname :wild-engine "test/"))
	       module-name
	       "/texture/"
	       texture-name))

(defun make-model-path (model-name &optional (format :obj))
  (concatenate 'string
	       (namestring (asdf:system-relative-pathname :wild-engine "test/"))
	       "model/models/"
	       model-name "/" model-name (case format
					   (:obj ".obj"))))

(defun make-model-texture-path (model-name)
  (concatenate 'string
	       (namestring (asdf:system-relative-pathname :wild-engine "test/"))
	       "model/models/"
	       model-name "/" model-name ".png"))
