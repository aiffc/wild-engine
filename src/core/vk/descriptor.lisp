(in-package :%wild-engine.core.vk)

(defun make-uniform-descriptor-pool-size ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-descriptor-pool-size
   :type :uniform-buffer
   :descriptor-count *uniform-buffer-size*))

(defun create-descriptor-pool ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (we.dbg:msg :app "create descriptor pool: -> ~%")
  (let* ((create-info (vk:make-descriptor-pool-create-info
		       :pool-sizes (list (make-uniform-descriptor-pool-size))
		       :max-sets *uniform-buffer-size*))
	 (pool (check-result #'vk:create-descriptor-pool *device* create-info)))
    (setf *descriptor-pool* pool)
    (we.dbg:msg :app "~2tcreate descriptor pool [~a] ~%" *descriptor-pool*)))

(defun destroy-descriptor-pool ()
  (we.dbg:msg :app "destroy instance: -> ~%")
  (when *descriptor-pool*
    (we.dbg:msg :app "~2tdestroy destroy pool [~a] ~%" *descriptor-pool*)
    (vk:destroy-descriptor-pool *device* *descriptor-pool*))
  (setf *uniform-buffer-size* 0))
