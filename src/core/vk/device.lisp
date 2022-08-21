(in-package :%wild-engine.core.vk)

(defun get-all-gpu-layers (sys)
  "function used to get all gpu layers"
  (remove-duplicates (mapcar #'vk:layer-name (vk:enumerate-device-layer-properties (get-gpu sys)))
		     :test #'string=))

(defun get-all-gpu-extensions (sys)
  "function used to get all gpu extensions"
  (remove-duplicates (append (mapcar #'vk:extension-name
				(vk:enumerate-device-extension-properties (get-gpu sys)))
			     (apply #'append
				    (mapcar (lambda (layer)
					 (mapcar #'vk:extension-name
					    (vk:enumerate-device-extension-properties (get-gpu sys) layer)))
				       (get-all-gpu-layers sys))))
		     :test #'string=))

(defun get-present-queue-family-indexs (sys)
  "function used to get present queue family indexs"
  (loop :for i :from 0 :below (length (get-gpu-device-queue-family-properties sys))
	:when (vk:get-physical-device-surface-support-khr (get-gpu sys) i (get-surface sys))
	  :collect i))

(defun get-property-queue-family-indexs (sys pro)
  "function used to get queue family indexs by pro"
  (let ((queue-falgs (mapcar #'vk:queue-flags (get-gpu-device-queue-family-properties sys))))
    (loop :for i :from 0 :below (length queue-falgs)
	  :for f := (nth i queue-falgs)
	  :when (find pro f :test #'eql)
	    :collect i)))

(defun make-device-queue-infos (&rest indexs)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  "function used to build device queue create info for device create info"
  (let ((rel-indexs (remove-duplicates indexs)))
    (mapcar (lambda (index)
	 ;; (let ((priorities (loop :for i :from 0 :below (vk:queue-count (nth index (get-gpu-device-queue-family-properties sys)))
	 ;; 			 :collect 0.0)))
	 ;;   (vk:make-device-queue-create-info
	 ;;    :queue-family-index index
	 ;;    :queue-priorities priorities))
	 (vk:make-device-queue-create-info
	    :queue-family-index index
	    :queue-priorities '(1.0)))            ;; just use one
       rel-indexs)))

(defun get-prop-index (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (values (first (get-present-queue-family-indexs sys))
	  (first (get-property-queue-family-indexs sys :graphics))
	  (first (get-property-queue-family-indexs sys :transfer))
	  (first (get-property-queue-family-indexs sys :compute))))

(defun device-create-info (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (multiple-value-bind (p-index g-index t-index c-index) (get-prop-index sys)
    (progn
      (set-device-pqindex sys p-index)
      (set-device-gqindex sys g-index)
      (set-device-cqindex sys c-index)
      (set-device-tqindex sys t-index))
    (vk:make-device-create-info
     :queue-create-infos (make-device-queue-infos p-index g-index t-index c-index)
     :enabled-layer-names nil
     :enabled-extension-names '("VK_KHR_swapchain"))))

;; (defun get-queues (device index pro)
;;   "function used to get queues by device and queue family index"
;;   (let ((count (vk:queue-count (nth index pro))))
;;     (loop :for i :from 0 :below count
;; 	  :collect (vk:get-device-queue device index i))))

(defun vk->init-device (sys)
  (we.dbg:msg :app "create device : ->~%")
  (let* ((all-extensions (get-all-gpu-extensions sys))
	 (all-layers (get-all-gpu-layers sys))
	 (create-info (device-create-info sys)))
    ;; check swapchain extension support
    (unless (find "VK_KHR_swapchain" all-extensions :test #'string=)
      (error "not support swapchain"))
    (setf (vk:enabled-features create-info) (get-gpu-features sys))
    ;; check debug layer
    (when (we.dbg:vk-debug-p)
      (when (find "VK_LAYER_KHRONOS_validation" all-layers :test #'string=)
	(pushnew "VK_LAYER_KHRONOS_validation" (vk:enabled-layer-names create-info) :test #'string=)))
    (let* ((device (check-result #'vk:create-device (get-gpu sys) create-info))
	   (g-queue (vk:get-device-queue device (get-device-gqindex sys) 0))
	   (t-queue (vk:get-device-queue device (get-device-tqindex sys) 0))
	   (c-queue (vk:get-device-queue device (get-device-cqindex sys) 0))
	   (p-queue (vk:get-device-queue device (get-device-pqindex sys) 0)))
      (progn
	(set-device sys device)
	(set-device-gqueue sys g-queue)
	(set-device-cqueue sys c-queue)
	(set-device-tqueue sys t-queue)
	(set-device-pqueue sys p-queue))
      (we.dbg:msg :app "~2tcreate device [~a]~%" device)
      (we.dbg:msg :app "~2tgraphics queue[~a] from family index [~a]~%" (get-device-gqueue) (get-device-gqindex sys))
      (we.dbg:msg :app "~2tpresent queue[~a] from family index [~a]~%"  (get-device-pqueue) (get-device-pqindex sys))
      (we.dbg:msg :app "~2ttransfer queue[~a] from family index [~a]~%" (get-device-tqueue) (get-device-tqindex sys))
      (we.dbg:msg :app "~2tcompute queue[~a] from family index [~a]~%"  (get-device-cqueue) (get-device-cqindex sys)))))

(defun vk->destroy-device (sys)
  (we.dbg:msg :app "destroy device: ->~%")
  (when (get-device sys)
    (we.dbg:msg :app "~2twaiting device [~a]~%" (get-device sys))
    (vk:device-wait-idle (get-device sys))
    (we.dbg:msg :app "~2tdestroy device [~a]~%" (get-device sys))
    (vk:destroy-device (get-device sys))
    (progn
      (set-device nil)
      (set-device-gqindex sys nil)
      (set-device-cqindex sys nil)
      (set-device-tqindex sys nil)
      (set-device-pqindex sys nil)
      (set-device-gqueue sys nil)
      (set-device-cqueue sys nil)
      (set-device-tqueue sys nil)
      (set-device-pqueue sys nil))))
