(in-package :%wile-engine.core.vulkan)

;; not support select gpu now,use the first gpu
(defun select-gpu (gpus)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (first gpus))

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.gpu) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(instance (%we.utils:instance chandle))
					(surface (%we.utils:surface chandle)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "pickup gpu: -> ~%")
  (let* ((gpus (vk:enumerate-physical-devices instance))
	 (gpu (select-gpu gpus)))
    (setf (%we.utils:gpu chandle) gpu
	  (%we.utils:gpu-capabilities chandle) (vk:get-physical-device-surface-capabilities-khr gpu surface)
	  (%we.utils:gpu-formats chandle) (vk:get-physical-device-surface-formats-khr gpu surface)
	  (%we.utils:gpu-present-mode chandle) (vk:get-physical-device-surface-present-modes-khr gpu surface)
	  (%we.utils:gpu-features chandle) (vk:get-physical-device-features gpu)
	  (%we.utils:gpu-properties chandle) (vk:get-physical-device-properties gpu)
	  (%we.utils:gpu-queue-families chandle) (vk:get-physical-device-queue-family-properties gpu)
	  (%we.utils:gpu-memory-infos chandle) (vk:get-physical-device-memory-properties gpu))
    (%we.dbg:msg :app "~2tselect gpu [~a ~a] ~%" gpu (vk:device-name (%we.utils:gpu-properties chandle)))))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.gpu))
  (declare (ignore handle))
  (%we.dbg:msg :app "nothing for destroy gpu~%"))
