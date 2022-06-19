(in-package :%wild-engine.core.vulkan)

(defun create-descriptor-pool (app pool-create-fun
			       &aux
				 (chandle (%we.utils:app-handle app))
				 (device (%we.utils:device chandle)))
  (when pool-create-fun
    (let ((pool-create-info (funcall pool-create-fun app)))
      (when pool-create-info
	(let ((pool (vk:create-descriptor-pool device pool-create-info)))
	  (%we.dbg:msg :app "create descriptor pool ~a~%" pool)
	  pool)))))

(defun destroy-descriptor-pool (app pool
				&aux
				 (chandle (%we.utils:app-handle app))
				  (device (%we.utils:device chandle)))
  (when pool
    (%we.dbg:msg :app "destroy descriptor pool ~a~%" pool)
    (vk:destroy-descriptor-pool device pool)))

(defun alloc-descriptor-sets (app set-layout pool texture-infos
			       &aux
				 (chandle (%we.utils:app-handle app))
				 (device (%we.utils:device chandle))
				 (image-count (length (%we.utils:swapchain-images chandle))))
  (when pool
    (let* ((alloc-info (vk:make-descriptor-set-allocate-info
			:descriptor-pool pool
			:set-layouts (loop :repeat image-count
					   :collect (nth 0 set-layout))))
	   (sets (vk:allocate-descriptor-sets device alloc-info))
	   (texture-infos (mapcar (lambda (fun)
			       (multiple-value-bind (name info binding) (funcall fun app)
				 (declare (ignore name))
				 (vk:make-write-descriptor-set
				  :dst-set nil
				  :dst-binding binding
				  :dst-array-element 0
				  :descriptor-type :combined-image-sampler
				  :image-info (list info))))
			     texture-infos)))
      (%we.dbg:msg :app "alloc descriptor sets ~a~%" sets)
      (labels ((bind-write-set (set)
		 (mapcar (lambda (info)
		      (setf (vk:dst-set info) set)
		      info)
		    texture-infos)))
	(loop :for i :from 0 :below image-count
	      :for set := (nth i sets)
	      :do (progn
		    (let ((write-info (append (get-uniform-buffer-infos app set i) (bind-write-set set))))
		      (vk:update-descriptor-sets device write-info nil))))
	sets))))

(defun free-descriptor-sets (app sets pool
			     &aux
			       (chandle (%we.utils:app-handle app))
			       (device (%we.utils:device chandle)))
  (when (and sets pool)
	(%we.dbg:msg :app "free descriptor sets ~a~%" sets)
	(vk:free-descriptor-sets device pool sets)))
