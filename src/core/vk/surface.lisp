(in-package :%wild-engine.core.vk)

(defun vk->init-surface (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "create surface: ->~%")
  (let ((surface (sdl-vulkan:sdl-create-surface (get-instance sys) (get-window sys))))
    (we.dbg:msg :app "~2tcreate surface ~a~%" surface)
    (set-surface sys surface)))

(defun vk->destroy-surface (sys)
  (we.dbg:msg :app "destroy surface: ->~%")
  (when (and (get-surface sys) (get-instance sys))
    (we.dbg:msg :app "~2tdestroy surface ~a~%" (get-surface sys))
    (vk:destroy-surface-khr (get-instance sys) (get-surface sys))
    (set-surface sys nil)))

