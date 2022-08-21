(in-package :wild-engine.math)

(defun ismat3p (val)
  (and (arrayp val)
       (eql (length val) (* 3 3))
       (every (lambda (num)
		(typep num 'float))
	      val)))

(deftype mat3 ()
  `(satisfies ismat3p))

(cffi:defcstruct (mat3 :class cmat3)
  (mat :float :count 9))

(defmethod cffi:translate-from-foreign (ptr (type cmat3))
  (apply #'m3:make (loop :for i :from 0 :below 9
			 :collect (we.u:to-single-float (cffi:mem-aref ptr :float i)))))

(defmethod cffi:translate-into-foreign-memory (value (type cmat3) ptr)
  (loop :for i :from 0 :below 9
	:do (setf (cffi:mem-aref ptr :float i)
		  (we.u:to-single-float (aref value i)))))

(defmethod cffi:expand-from-foreign (ptr (type cmat3))
  `(apply #'m3:make (loop :for i :from 0 :below 9
			  :collect (we.u:to-single-float (cffi:mem-aref ,ptr :float i)))))

(defmethod cffi:expand-into-foreign-memory (value (type cmat3) ptr)
  `(loop :for i :from 0 :below 9
	 :do (setf (cffi:mem-aref ,ptr :float i)
		   (we.u:to-single-float (aref ,value i)))))

(defun alloc-mat3 (m3)
  (cffi:foreign-alloc '(:struct mat3) :initial-element m3))
