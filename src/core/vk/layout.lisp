(in-package :%wild-engine.core.vulkan)

(defparameter *layout-hash* (make-hash-table))

(defun layout-create-info (layout-bindings)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if layout-bindings
      (vk:make-descriptor-set-layout-create-info :bindings layout-bindings)
      nil))

(defun create-layout (app layout-info
		      &aux
			(chandle (%we.utils:app-handle app))
			(device (%we.utils:device chandle)))
  "ready to do"
  (let* ((layout-infos (funcall layout-info))
	 (set-layout (if layout-infos (list (vk:create-descriptor-set-layout device layout-infos)) nil))      ;; list 
	 (pipeline-layout-create-info (vk:make-pipeline-layout-create-info
				       :set-layouts set-layout))
	 (layout (vk:create-pipeline-layout device pipeline-layout-create-info)))
    (when layout-infos
      (%we.dbg:msg :app "create descriptor set layout ~a~%" set-layout))
    (%we.dbg:msg :app "create layout ~a~%" layout)
    (values layout set-layout)))

(defun destroy-layout (app layout set-layout
		       &aux
			 (chandle (%we.utils:app-handle app))
			 (device (%we.utils:device chandle)))
  "ready to do"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when set-layout
    (%we.dbg:msg :app "destroy descriptor-sets ~a~%" set-layout)
    (vk:destroy-descriptor-set-layout device (first set-layout)))  ;; to do
  (when layout
    (%we.dbg:msg :app "destroy layout ~a~%" layout)
    (vk:destroy-pipeline-layout device layout)))
