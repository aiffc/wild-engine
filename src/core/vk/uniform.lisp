(in-package :%wild-engine.core.vulkan)

(defparameter *uniform-hash* (make-hash-table))

(defun create-uniform-buffer (app size
			      &aux
				(chandle (%we.utils:app-handle app))
				(images (%we.utils:swapchain-images chandle)))
  
  (let* ((ulist (loop :for i :from 0 :below (length images)
		      :collect (multiple-value-bind (buffer memory)
				   (create-buffer app size :uniform-buffer '(:host-visible :host-coherent))
				 (%we.dbg:msg :app "create uniform buffer ~a ~a~%" buffer memory)
				 (list buffer memory))))
	 (buffers (mapcar #'first ulist))
	 (memories (mapcar #'second ulist)))
    (push (list :buffers buffers
		:memories memories)
	  (gethash app *uniform-hash*))
    
    memories))

(defun destroy-uniform-buffer (app
			       &aux
				 (uniform (gethash app *uniform-hash*)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (format t "~a~%" uniform)
  (when uniform
    (mapcar (lambda (u)
	 ;; (mapcar (lambda (buffer)
	 ;;      (%we.dbg:msg :app "destroy uniform buffer ~a~%" buffer)
	 ;;      (destroy-buffer app buffer))
	 ;;    (getf u :buffers))
	 ;; (mapcar (lambda (memory)
	 ;;      (%we.dbg:msg :app "free uniform memory ~a~%" memory)
	 ;;      (free-memory app memory))
	 ;;    (getf u :memories))
	 )
       uniform))
  (setf (gethash app *uniform-hash*) nil))
