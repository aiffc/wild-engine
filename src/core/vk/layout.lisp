(in-package :%wild-engine.core.vulkan)

(defun layout-create-info (layouts)
  "ready to do"
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (ignore layouts))
  (vk:make-pipeline-layout-create-info))

(defun create-layout (app layout-info
		      &aux
			(chandle (%we.utils:app-handle app))
			(device (%we.utils:device chandle)))
  "ready to do"
  (let ((layout (vk:create-pipeline-layout device (funcall layout-info))))
    (%we.dbg:msg "create layout ~a~%" layout)
    layout))

(defun destroy-layout (app layout
		       &aux
			 (chandle (%we.utils:app-handle app))
			 (device (%we.utils:device chandle)))
  "ready to do"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when layout
    (%we.dbg:msg "destroy layout ~a~%" layout)
    (vk:destroy-pipeline-layout device layout)))

;; (defun descriptor-set-layout-create-info ()
;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
;;   (list (vk:make-descriptor-set-layout-create-info
;; 	 :bindings (list (vk:make-descriptor-set-layout-binding    ;; for mvp
;; 			  :descriptor-type :uniform-buffer
;; 			  :descriptor-count 1
;; 			  :stage-flags :vertex)))))

;; (defun descriptor-pool-create-info ()
;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
;;   (vk:make-descriptor-pool-create-info
;;    :pool-sizes (list (vk:make-descriptor-pool-size
;; 		      :type :uniform-buffer
;; 		      :descriptor-count 1))
;;    :max-sets 1))

;; (defmethod %we.utils:make-app :after (app (handle %we.utils:vk.layout) args
;; 				      &aux
;; 					(chandle (%we.utils:app-handle app))
;; 					(device (%we.utils:device chandle)))
;;   (declare (ignore handle args device))
;;   (%we.dbg:msg :app "create layout : ->~%")
;;   (let* ((pool-create-info (descriptor-pool-create-info))
;; 	 (pool-handle (check-result #'vk:create-descriptor-pool device pool-create-info))
;; 	 (dsc-create-info (descriptor-set-layout-create-info))
;; 	 (dscl-handle (check-result #'vk:create-descriptor-set-layout device dsc-create-info))
;; 	 (layout-create-info (vk:make-pipeline-layout-create-info
;; 			      :set-layouts (list dscl-handle)))
;; 	 (layout (check-result #'vk:create-pipeline-layout device layout-create-info)))
;;     (setf (%we.utils:layout chandle) layout        ;; not list !!
;; 	  (%we.utils:layout-descriptor-pool chandle) pool-handle
;; 	  (%we.utils:layout-descriptor-set-layout chandle) dscl-handle)))

;; (defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.layout)
;; 					  &aux
;; 					    (chandle (%we.utils:app-handle app))
;; 					    (device (%we.utils:device chandle))
;; 					    (dscl-handle (%we.utils:layout-descriptor-set-layout chandle))
;; 					    (layout (%we.utils:layout chandle))
;; 					    (pool-handle (%we.utils:layout-descriptor-pool chandle)))
;;   (declare (ignore handle device dscl-handle layout pool-handle))
;;   (%we.dbg:msg :app "destroy layout: ->~%")
;;   (when dscl-handle
;;     (%we.dbg:msg :app "destroy descriptor set layout ~a~%" dscl-handle)
;;     (vk:destroy-descriptor-set-layout device dscl-handle))
;;   (when layout
;;     (%we.dbg:msg :app "destroy pipeline layout ~a~%" layout)
;;     (vk:destroy-pipeline-layout device layout))
;;   (when pool-handle
;;     (%we.dbg:msg :app "destroy descriptor pool ~a~%" pool-handle)
;;     (vk:destroy-descriptor-pool device pool-handle)))
