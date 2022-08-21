(in-package :wild-engine.math)

(defun ismat2p (val)
  (and (arrayp val)
       (eql (length val) (* 2 2))
       (every (lambda (num)
		(typep num 'float))
	      val)))

(deftype mat2 ()
  `(satisfies ismat2p))

(cffi:defcstruct (mat2 :class cmat2)
  (mat :float :count 4))

(defmethod cffi:translate-from-foreign (ptr (type cmat2))
  (apply #'m2:make (loop :for i :from 0 :below 4
			 :collect (we.u:to-single-float (cffi:mem-aref ptr :float i)))))

(defmethod cffi:translate-into-foreign-memory (value (type cmat2) ptr)
  (loop :for i :from 0 :below 4
	:do (setf (cffi:mem-aref ptr :float i)
		  (we.u:to-single-float (aref value i)))))

(defmethod cffi:expand-from-foreign (ptr (type cmat2))
  `(apply #'m2:make (loop :for i :from 0 :below 4
			  :collect (we.u:to-single-float (cffi:mem-aref ,ptr :float i)))))

(defmethod cffi:expand-into-foreign-memory (value (type cmat2) ptr)
  `(loop :for i :from 0 :below 4
	 :do (setf (cffi:mem-aref ,ptr :float i)
		   (we.u:to-single-float (aref ,value i)))))

(defun alloc-mat2 (m2)
  (cffi:foreign-alloc '(:struct mat2) :initial-element m2))
