(in-package :%wild-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.pipeline-cache) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(device (%we.utils:device chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "create pipeline cache : ->~%")
  (let ((cache (vk:create-pipeline-cache device (vk:make-pipeline-cache-create-info))))
    (%we.dbg:msg :app "~t ~a~%" cache)
    (setf (%we.utils:pipeline-cache chandle) cache)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.pipeline-cache)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (device (%we.utils:device chandle))
					    (cache (%we.utils:pipeline-cache chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy pipeline cache: ->~%")
  (when cache
    (%we.dbg:msg :app "~t ~a~%" cache)
    (vk:destroy-pipeline-cache device cache)))

