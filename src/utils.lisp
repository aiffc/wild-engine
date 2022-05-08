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
