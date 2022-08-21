(in-package :%wild-engine.core.vk)

;; not support select gpu now,use the first gpu
(defun select-gpu (gpus)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (first gpus))

(defun vk->init-gpu (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "pickup gpu: -> ~%")
  (let* ((gpus (vk:enumerate-physical-devices (get-instance sys)))
	 (gpu (select-gpu gpus)))
    (progn
      (set-gpu sys gpu)
      (set-gpu-surface-capabilities sys (vk:get-physical-device-surface-capabilities-khr gpu (get-surface sys)))
      (set-gpu-formats sys (vk:get-physical-device-surface-formats-khr gpu (get-surface sys)))
      (set-gpu-present-modes sys (vk:get-physical-device-surface-present-modes-khr gpu (get-surface sys)))
      (set-gpu-features sys (vk:get-physical-device-features gpu))
      (set-gpu-properties sys (vk:get-physical-device-properties gpu))
      (set-gpu-device-queue-family-properties sys (vk:get-physical-device-queue-family-properties gpu))
      (set-gpu-memory-properties sys (vk:get-physical-device-memory-properties gpu)))
    (we.dbg:msg :app "~2tselect gpu [~a ~a] ~%" gpu (vk:device-name (get-gpu-properties sys)))))

(defun vk->destroy-gpu (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "nothing for destroy gpu~%")
  (progn
    (set-gpu sys nil)
    (set-gpu-surface-capabilities sys nil)
    (set-gpu-formats sys nil)
    (set-gpu-present-modes sys nil)
    (set-gpu-features sys nil)
    (set-gpu-properties sys nil)
    (set-gpu-device-queue-family-properties sys nil)
    (set-gpu-memory-properties sys nil)))

