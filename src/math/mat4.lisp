(in-package :%wild-engine.math)

(cffi:defcstruct (mat4 :class cmat4)
  (mat :float :count 16))

(defmethod cffi:translate-from-foreign (ptr (type cmat4))
  (apply #'m4:make (loop :for i :from 0 :below 16
			 :collect (we.u:to-single-float (cffi:mem-aref ptr :float i)))))

(defmethod cffi:translate-into-foreign-memory (value (type cmat4) ptr)
  (loop :for i :from 0 :below 16
	:do (setf (cffi:mem-aref ptr :float i)
		  (we.u:to-single-float (aref value i)))))

(defmethod cffi:expand-from-foreign (ptr (type cmat4))
  `(apply #'m4:make (loop :for i :from 0 :below 16
			  :collect (we.u:to-single-float (cffi:mem-aref ,ptr :float i)))))

(defmethod cffi:expand-into-foreign-memory (value (type cmat4) ptr)
  `(loop :for i :from 0 :below 16
	 :do (setf (cffi:mem-aref ,ptr :float i)
		   (we.u:to-single-float (aref ,value i)))))

(defun alloc-mat4 (m4)
  (cffi:foreign-alloc '(:struct mat4) :initial-element m4))
