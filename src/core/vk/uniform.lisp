(in-package :%wild-engine.core.vulkan)

(defparameter *uniform-hash* (make-hash-table))

(defun create-uniform-buffer (app name size dst-binding
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
    (push (list :name name
		:binding dst-binding
		:size size
		:buffers buffers
		:memories memories)
	  (gethash app *uniform-hash*))
    memories))

(defun get-uniform-buffer-infos (app set index
				 &aux
				   (uniform-buffers (gethash app *uniform-hash*)))
  "function use to generate buffer infos that all uniform buffer created"
  (loop :for info :in uniform-buffers
	:collect (vk:make-write-descriptor-set
		  :dst-set set
		  :dst-binding (getf info :binding)
		  :dst-array-element 0
		  :descriptor-type :uniform-buffer
		  :buffer-info (list (vk:make-descriptor-buffer-info
				      :buffer (nth index (getf info :buffers))
				      :offset 0
				      :range (getf info :size))))))

(defun slot-uniform (app name index &optional (slot-type :buffers)
		     &aux
		       (uniform-buffers (gethash app *uniform-hash*)))
  "get memory or buffer by index"
  (nth index
       (getf (find name uniform-buffers
		   :key (lambda (u)
			  (getf u :name))
		   :test #'eql)
	     slot-type)))

(defun map-uniform (app name ptr size index
		    &aux
		      (memory (slot-uniform app name index :memories)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%we.dbg:msg :app-high "map uniform data to memory ~a~%" memory)
  (map-memory app memory ptr size))

(defun destroy-uniform-buffer (app
			       &aux
				 (uniform (gethash app *uniform-hash*)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; free it in free resource
  (%we.dbg:msg :app "~a~%" uniform)
  ;; (when uniform
  ;;   (mapcar (lambda (u)
  ;; 	 ;; (mapcar (lambda (buffer)
  ;; 	 ;;      (%we.dbg:msg :app "destroy uniform buffer ~a~%" buffer)
  ;; 	 ;;      (destroy-buffer app buffer))
  ;; 	 ;;    (getf u :buffers))
  ;; 	 ;; (mapcar (lambda (memory)
  ;; 	 ;;      (%we.dbg:msg :app "free uniform memory ~a~%" memory)
  ;; 	 ;;      (free-memory app memory))
  ;; 	 ;;    (getf u :memories))
  ;; 	 )
  ;;      uniform))
  (setf (gethash app *uniform-hash*) nil))
