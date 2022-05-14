(in-package :wild-engine.api)

(defmacro define-vertex (name &rest context)
  (let ((vertex-initial-fun (we.u:create-symbol 'createv- name)))
    `(progn
       (eval-when (:compile-toplevel :execute :load-toplevel))
       (let ((,name (vector ,@context)))
	 (defun ,vertex-initial-fun (app)
	   (let ((size (length ,name)))
	     (cffi:with-foreign-object (ptr '(:struct %we.vk:vertex) size)
	       (loop :for i from 0 :below size
		     :do (setf (cffi:mem-aref ptr '(:struct %we.vk:vertex) i)
			       (aref ,name i)))
	       (%we.vk:create-vertex-buffer app ptr size))))))))

(defun make-vertex (&key
		      (v (v3:make 0.0 0.0 0.0))
		      (vt (v3:make 0.0 0.0 0.0))
		      (vn (v3:make 0.0 0.0 0.0)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-instance '%we.vk:vertex :vertex-v v
				:vertex-vt vt
				:vertex-vn vn))
