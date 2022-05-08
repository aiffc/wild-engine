(in-package :%wild-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.cmd-pool) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(device (%we.utils:device chandle))
					(gfindex (%we.utils:device-gqueue-family-index chandle))
					(cfindex (%we.utils:device-cqueue-family-index chandle)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "create cmd-pool : ->~%")
  (let* ((cpg (check-result #'vk:create-command-pool
			    device
			    (vk:make-command-pool-create-info
			     :flags :reset-command-buffer
			     :queue-family-index gfindex)))
	 (cpc (check-result #'vk:create-command-pool
			    device
			    (vk:make-command-pool-create-info
			     :flags :reset-command-buffer
			     :queue-family-index cfindex))))
    (%we.dbg:msg :app "~2tcreate graphics command pool [~a]~%" cpg)
    (%we.dbg:msg :app "~2tcreate compute command pool [~a]~%" cpc)
    (setf (%we.utils:cmd-pool-graphics chandle) cpg
	  (%we.utils:cmd-pool-compute chandle) cpc)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.cmd-pool)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (device (%we.utils:device chandle))
					    (cpg (%we.utils:cmd-pool-graphics chandle))
					    (cpc (%we.utils:cmd-pool-compute chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy cmd-pool: ->~%")
  (when cpg
    (%we.dbg:msg :app "~2tdestroy graphics command pool [~a]~%" cpg)
    (vk:destroy-command-pool device cpg))
  (when cpc
    (%we.dbg:msg :app "~2tdestroy compute command pool [~a]~%" cpc)
    (vk:destroy-command-pool device cpc)))
