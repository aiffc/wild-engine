(in-package :%wild-engine.core.vulkan)

(defun get-all-gpu-layers (gpu)
  "function used to get all gpu layers"
  (remove-duplicates (mapcar #'vk:layer-name (vk:enumerate-device-layer-properties gpu))
		     :test #'string=))

(defun get-all-gpu-extensions (gpu)
  "function used to get all gpu extensions"
  (remove-duplicates (append (mapcar #'vk:extension-name
				(vk:enumerate-device-extension-properties gpu))
			     (apply #'append
				    (mapcar (lambda (layer)
					 (mapcar #'vk:extension-name
					    (vk:enumerate-device-extension-properties gpu layer)))
				       (get-all-gpu-layers gpu))))
		     :test #'string=))

(defun get-present-queue-family-indexs (info gpu surface)
  "function used to get present queue family indexs"
  (loop :for i :from 0 :below (length info)
	:when (vk:get-physical-device-surface-support-khr gpu i surface)
	  :collect i))

(defun get-property-queue-family-indexs (info pro)
  "function used to get queue family indexs by pro"
  (let ((queue-falgs (mapcar #'vk:queue-flags info)))
    (loop :for i :from 0 :below (length queue-falgs)
	  :for f := (nth i queue-falgs)
	  :when (find pro f :test #'eql)
	    :collect i)))

(defun make-device-queue-infos (pro &rest indexs)
  "function used to build device queue create info for device create info"
  (let ((rel-indexs (remove-duplicates indexs)))
    (mapcar (lambda (index)
	 (let ((priorities (loop :for i :from 0 :below (vk:queue-count (nth index pro))
				 :collect 0.0)))
	   (vk:make-device-queue-create-info
	    :queue-family-index index
	    :queue-priorities priorities)))
       rel-indexs)))

(defun get-queues (device index pro)
  "function used to get queues by device and queue family index"
  (let ((count (vk:queue-count (nth index pro))))
    (loop :for i :from 0 :below count
	  :collect (vk:get-device-queue device index i))))

(define-condition swapchain-not-support (error)
  ()
  (:report (lambda (obj stream)
	     (declare (ignore obj))
	     (format stream "not support swapchain extesion~%"))))

(defun device-create-info (queue-infos)
  (vk:make-device-create-info
   :queue-create-infos queue-infos
   :enabled-layer-names nil
   :enabled-extension-names '("VK_KHR_swapchain")))

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.device) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(queue-families (%we.utils:gpu-queue-families chandle))
					(surface (%we.utils:surface chandle))
					(gpu (%we.utils:gpu chandle))
					(gpu-feature (%we.utils:gpu-features chandle)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "create device : ->~%")
  (let* ((p-indexs (get-present-queue-family-indexs queue-families gpu surface))
	 (g-indexs (get-property-queue-family-indexs queue-families :graphics))
	 (t-indexs (get-property-queue-family-indexs queue-families :transfer))
	 (c-indexs (get-property-queue-family-indexs queue-families :compute))
	 (p-index (first p-indexs))
	 (g-index (first g-indexs))
	 (t-index (first t-indexs))
	 (c-index (first c-indexs))
	 (all-extensions (get-all-gpu-extensions gpu))
	 (all-layers (get-all-gpu-layers gpu))
	 (queue-infos (make-device-queue-infos queue-families p-index g-index t-index c-index))
	 (create-info (device-create-info queue-infos)))
    ;; check swapchain support
    (unless (find "VK_KHR_swapchain" all-extensions :test #'string=)
      (error 'swapchain-not-support))
    (setf (vk:enabled-features create-info) gpu-feature)
    ;; check debug layer
    (when (%we.dbg:vk-debug-p)
      (when (find "VK_LAYER_KHRONOS_validation" all-layers :test #'string=)
	(pushnew "VK_LAYER_KHRONOS_validation" (vk:enabled-layer-names create-info) :test #'string=)))
    (let* ((device (check-result #'vk:create-device gpu create-info))
	   (g-queues (get-queues device g-index queue-families))
	   (t-queues (get-queues device t-index queue-families))
	   (c-queues (get-queues device c-index queue-families))
	   (p-queues (get-queues device p-index queue-families)))
      (%we.dbg:msg :app "~2tcreate device [~a]~%" device)
      (%we.dbg:msg :app "~2tgraphics queue[~a] from family index [~a]~%" g-queues g-index)
      (%we.dbg:msg :app "~2tpresent queue[~a] from family index [~a]~%" p-queues p-index)
      (%we.dbg:msg :app "~2ttransfer queue[~a] from family index [~a]~%" t-queues t-index)
      (%we.dbg:msg :app "~2tcompute queue[~a] from family index [~a]~%" c-queues c-index)
      (setf (%we.utils:device chandle) device
	    (%we.utils:device-gqueue-family-index chandle) g-index
	    (%we.utils:device-cqueue-family-index chandle) c-index
	    (%we.utils:device-tqueue-family-index chandle) t-index
	    (%we.utils:device-pqueue-family-index chandle) p-index
	    (%we.utils:device-gqueues chandle) g-queues
	    (%we.utils:device-cqueues chandle) c-queues
	    (%we.utils:device-tqueues chandle) t-queues
	    (%we.utils:device-pqueues chandle) p-queues))))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.device)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (device (%we.utils:device chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy device: ->~%")
  (when device
    (%we.dbg:msg :app "~2twaiting device [~a]~%" device)
    (vk:device-wait-idle device)
    (%we.dbg:msg :app "~2tdestroy device [~a]~%" device)
    (vk:destroy-device device)))
