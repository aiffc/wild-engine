(in-package :%wild-engine.core.vk)

(defun vk->init-cmd-pool (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "create cmd-pool : ->~%")
  (progn
    (set-cmd-pool-graphics sys
			   (check-result #'vk:create-command-pool
					 (get-device sys)
					 (vk:make-command-pool-create-info
					  :flags :reset-command-buffer
					  :queue-family-index (get-device-gqindex sys))))
    (set-cmd-pool-compute sys
			  (check-result #'vk:create-command-pool
					(get-device sys)
					(vk:make-command-pool-create-info
					 :flags :reset-command-buffer
					 :queue-family-index (get-device-cqindex sys)))))
  (we.dbg:msg :app "~2tcreate graphics command pool [~a]~%" (get-cmd-pool-graphics sys))
  (we.dbg:msg :app "~2tcreate compute command pool [~a]~%" (get-cmd-pool-compute sys)))

(defun vk->destroy-cmd-pool (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "destroy cmd-pool: ->~%")
  (when (get-cmd-pool-graphics sys)
    (we.dbg:msg :app "~2tdestroy graphics command pool [~a]~%" (get-cmd-pool-graphics sys))
    (vk:destroy-command-pool (get-device sys) (get-cmd-pool-graphics sys))
    (set-cmd-pool-graphics sys nil))
  (when (get-cmd-pool-compute sys)
    (we.dbg:msg :app "~2tdestroy compute command pool [~a]~%" (get-cmd-pool-compute sys))
    (vk:destroy-command-pool (get-device sys) (get-cmd-pool-compute sys))
    (set-cmd-pool-compute sys nil)))
