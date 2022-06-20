(in-package :wild-engine.api)

(defmacro define-model (name model-path)
  (let ((fun-name (we.u:create-symbol 'createm- name))
	(path (eval `,model-path)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (defun ,fun-name (app)
	 (let* ((model-data (%we.model:load-model ,path))
		(size (length model-data))
		(vsize (%we.vk:vertex-size))
		(isize (cffi:foreign-type-size :uint32)))
	   (cffi:with-foreign-objects ((vptr '(:struct %we.vk:vertex) size)
				       (iptr :uint32 size))
	     (loop :for i :from 0 :below size
		   :for d := (aref model-data i)
		   :do (progn
			 (setf (cffi:mem-aref vptr '(:struct %we.vk:vertex) i) d
			       (cffi:mem-aref iptr :uint32 i) i)))
	     (we.u:with-mvalues (((vbuffer) (%we.vk:create-vertex-buffer app vptr size))
				 ((ibuffer icount) (%we.vk:create-index-buffer app iptr size)))
	       (values vbuffer ibuffer icount))))))))

