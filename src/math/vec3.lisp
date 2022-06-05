(in-package :%wild-engine.math)

(defun isvec3p (val)
  (and (arrayp val)
       (eql (length val) 3)
       (every (lambda (num)
		(typep num 'float))
	      val)))

(deftype vec3 ()
  `(satisfies isvec3p))

(cffi:defcstruct (vec3 :class cvec3)
  (x :float)
  (y :float)
  (z :float))

(defmethod cffi:translate-from-foreign (ptr (type cvec3))
  (cffi:with-foreign-slots ((x y z) ptr (:struct vec3))
    (v3:make (we.u:to-single-float x)
	     (we.u:to-single-float y)
	     (we.u:to-single-float z))))

(defmethod cffi:translate-into-foreign-memory (value (type cvec3) ptr)
  (cffi:with-foreign-slots ((x y z) ptr (:struct vec3))
    (setf x (we.u:to-single-float (aref value 0))
	  y (we.u:to-single-float (aref value 1))
	  z (we.u:to-single-float (aref value 2)))))

(defmethod cffi:expand-from-foreign (ptr (type cvec3))
  `(cffi:with-foreign-slots ((x y z) ,ptr (:struct vec3))
     (v3:make (we.u:to-single-float x)
	      (we.u:to-single-float y)
	      (we.u:to-single-float z))))

(defmethod cffi:expand-into-foreign-memory (value (type cvec3) ptr)
  `(cffi:with-foreign-slots ((x y z) ,ptr (:struct vec3))
    (setf x (coerce (aref ,value 0) 'double-float)
	  y (coerce (aref ,value 1) 'double-float)
	  z (coerce (aref ,value 2) 'double-float))))

(defun alloc-vec3 (vec3)
  "alloc a vectory [v3:make]"
  (cffi:foreign-alloc '(:struct vec3) :initial-element vec3))

(defun alloc-vec3-xyz (x y z)
  "alloc a vectory by x y z"
  (cffi:foreign-alloc '(:struct vec3) :initial-element (v3:make x y z)))
