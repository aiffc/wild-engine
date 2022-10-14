(in-package :wild-engine.math)

(defun isvec4p (val)
  (and (arrayp val)
       (eql (length val) 4)
       (every (lambda (num)
		(typep num 'float))
	      val)))

(deftype vec4 ()
  `(satisfies isvec4p))

(cffi:defcstruct (vec4 :class cvec4)
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defmethod cffi:translate-from-foreign (ptr (type cvec4))
  (cffi:with-foreign-slots ((x y z w) ptr (:struct vec4))
    (v4:make (we.u:to-single-float x)
	     (we.u:to-single-float y)
	     (we.u:to-single-float z)
	     (we.u:to-single-float w))))

(defmethod cffi:translate-into-foreign-memory (value (type cvec4) ptr)
  (cffi:with-foreign-slots ((x y z w) ptr (:struct vec4))
    (setf x (we.u:to-single-float (aref value 0))
	  y (we.u:to-single-float (aref value 1))
	  z (we.u:to-single-float (aref value 2))
	  w (we.u:to-single-float (aref value 3)))))

(defmethod cffi:expand-from-foreign (ptr (type cvec4))
  `(cffi:with-foreign-slots ((x y z w) ,ptr (:struct vec4))
     (v4:make (we.u:to-single-float x)
	      (we.u:to-single-float y)
	      (we.u:to-single-float z)
	      (we.u:to-single-float w))))

(defmethod cffi:expand-into-foreign-memory (value (type cvec4) ptr)
  `(cffi:with-foreign-slots ((x y z w) ,ptr (:struct vec4))
     (setf x (coerce (aref ,value 0) 'double-float)
	   y (coerce (aref ,value 1) 'double-float)
	   z (coerce (aref ,value 2) 'double-float)
	   w (coerce (aref ,value 3) 'double-float))))

(defun alloc-vec4 (vec4)
  "alloc a vectory [v4:make]"
  (cffi:foreign-alloc '(:struct vec4) :initial-element vec4))

(defun alloc-vec4-xyzw (x y z w)
  "alloc a vectory by x y z"
  (cffi:foreign-alloc '(:struct vec4) :initial-element (v4:make x y z w)))
