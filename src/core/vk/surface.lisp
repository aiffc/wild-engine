(in-package :%wild-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.surface) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(window (%we.utils:window chandle))
					(instance (%we.utils:instance chandle)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "create surface: ->~%")
  (let ((surface (sdl-vulkan:sdl-create-surface instance window)))
    (%we.dbg:msg :app "~2tcreate surface ~a~%" surface)
    (setf (%we.utils:surface chandle) surface)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.surface)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (instance (%we.utils:instance chandle))
					    (surface (%we.utils:surface chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy surface: ->~%")
  (when surface
    (%we.dbg:msg :app "~2tdestroy surface ~a~%" surface)
    (vk:destroy-surface-khr instance surface)))
