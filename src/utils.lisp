(in-package :wild-engine.utils)

(defun create-symbol (&rest names)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (values (intern (format nil "~{~a~}" names))))

(declaim (inline set-value))

(defun set-value (lst key dval
		  &aux
		    (lval (getf lst key)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if lval lval dval))

(defun to-single-float (x)
  (coerce x 'single-float))

(defun my-free (val)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (cffi:pointerp val)
    (cffi:foreign-free val)))

(defmacro with-cffi-alloc ((&rest args) &body body)
  "my with foreign object"
  (destructuring-bind (&optional first-handle . rest-handle) args
    (let ((handle (first first-handle) )
	  (alloc-info (second first-handle)))
      (typecase handle
	(null `(progn ,@body))
	(atom `(let ((,handle ,alloc-info))
		 ,(if rest-handle
		      `(with-cffi-alloc ,rest-handle ,@body)
		      `(progn ,@body))
		 (my-free ,handle)))))))

(cffi:defctype size-t #.(if (= 4 (cffi:foreign-type-size :pointer)) :uint32 :uint64))

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer)
  (src :pointer)
  (count size-t))
