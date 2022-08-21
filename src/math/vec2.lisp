(in-package :wild-engine.math)

(defun isvec2p (val)
  (and (arrayp val)
       (eql (length val) 2)
       (every (lambda (num)
		(typep num 'float))
	      val)))

(deftype vec2 ()
  `(satisfies isvec2p))

(cffi:defcstruct (vec2 :class cvec2)
  (x :float)
  (y :float))

(defmethod cffi:translate-from-foreign (ptr (type cvec2))
  (cffi:with-foreign-slots ((x y) ptr (:struct vec2))
    (v2:make (we.u:to-single-float x)
	     (we.u:to-single-float y))))

(defmethod cffi:translate-into-foreign-memory (value (type cvec2) ptr)
  (cffi:with-foreign-slots ((x y) ptr (:struct vec2))
    (setf x (we.u:to-single-float (aref value 0))
	  y (we.u:to-single-float (aref value 1)))))

(defmethod cffi:expand-from-foreign (ptr (type cvec2))
  `(cffi:with-foreign-slots ((x y) ,ptr (:struct vec2))
     (v2:make (we.u:to-single-float x)
	      (we.u:to-single-float y))))

(defmethod cffi:expand-into-foreign-memory (value (type cvec2) ptr)
  `(cffi:with-foreign-slots ((x y) ,ptr (:struct vec2))
     (setf x (coerce (aref ,value 0) 'double-float)
	   y (coerce (aref ,value 1) 'double-float))))

(defun alloc-vec2 (vec2)
  "alloc a vectory [v2:make]"
  (cffi:foreign-alloc '(:struct vec2) :initial-element vec2))

(defun alloc-vec2-xy (x y)
  "alloc a vectory by x y"
  (cffi:foreign-alloc '(:struct vec2) :initial-element (v2:make x y)))
