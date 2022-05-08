(in-package :%wild-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.signal) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(device (%we.utils:device chandle)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "create signal : ->~%")
  (let* ((create-info (vk:make-semaphore-create-info))
	 (image-available (list (vk:create-semaphore device create-info)))
	 (render-finish (list (vk:create-semaphore device create-info))))
    (%we.dbg:msg :app "~2tcreate image available semaphore [~a] in device [~a]~%" image-available device)
    (%we.dbg:msg :app "~2tcreate render finish semaphore [~a] in device [~a]~%" render-finish device)
    (setf (%we.utils:signal-image-available chandle) image-available
	  (%we.utils:signal-render-finish chandle) render-finish)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.signal)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (device (%we.utils:device chandle))
					    (image-available (%we.utils:signal-image-available chandle))
					    (render-finish (%we.utils:signal-render-finish chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy signal: ->~%")
  (when image-available
    (mapcar (lambda (s)
	 (%we.dbg:msg :app "~4tdestroy semaphore[~a] in device[~a]~%" s device)
	 (vk:destroy-semaphore device s))
       image-available))
  (when render-finish
    (mapcar (lambda (s)
	 (%we.dbg:msg :app "~4tdestroy semaphore[~a] in device[~a]~%" s device)
	 (vk:destroy-semaphore device s))
       render-finish)))
