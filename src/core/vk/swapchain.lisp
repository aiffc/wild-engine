(in-package :%wild-engine.core.vk)

(defun get-present-mode ()
  "not support select present-mode now ready to do"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  :fifo-khr)

(defun get-min-image-count (sys)
  (let* ((max-image-count (vk:max-image-count (get-gpu-surface-capabilities sys)))
	 (min-image-count (vk:min-image-count (get-gpu-surface-capabilities sys)))
	 (image-count (1+ min-image-count)))
    (if (and (> max-image-count 0)
	     (> image-count max-image-count))
	max-image-count
	image-count)))

(defun get-format (sys format)
  (let* ((f (find format (get-gpu-formats sys) :key #'vk:format)))
    (if f
	f
	(progn
	  (warn "not support format you selected use the default format~%")
	  (first (get-gpu-formats sys))))))

(defun get-composite-alpha (sys)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((default :inherit)
	(composite-alphas (vk:supported-composite-alpha (get-gpu-surface-capabilities sys))))
    (if (member :opaque composite-alphas)
	:opaque
	default)))

(defun get-pre-transform (sys)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((current-transform (vk:current-transform (get-gpu-surface-capabilities sys)))
	(supported-transform (vk:supported-transforms (get-gpu-surface-capabilities sys))))
    (if (member :identity supported-transform)
	:identity
	current-transform)))

(defun %create-image-view (sys image format aspect mip-level)
  "function used to create a single image view"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((create-info (vk:make-image-view-create-info
		       :subresource-range (vk:make-image-subresource-range
					   :layer-count 1
					   :base-array-layer 0
					   :level-count mip-level
					   :base-mip-level 0
					   :aspect-mask aspect)
		       :components (vk:make-component-mapping
				    :r :r
				    :g :g
				    :b :b
				    :a :a)
		       :format format
		       :view-type :2d
		       :image image))
	 (image-view (check-result #'vk:create-image-view (get-device sys) create-info)))
    image-view))

(defun create-image-view (sys images format)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  "function used to create imageviews"
  (mapcar (lambda (image)
       (%create-image-view sys image format :color 1))
     images))

(defun create-swapchain (sys format)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "function used to create a swapchain"
  (we.dbg:msg :app "create swapchain : ->~%")
  (let* ((select-format (get-format sys format))
	 (create-info (vk:make-swapchain-create-info-khr
		       :surface (get-surface sys)
		       :min-image-count (get-min-image-count sys)
		       :image-format (vk:format select-format)
		       :image-color-space (vk:color-space select-format)
		       :image-extent (sdl-vulkan:sdl-get-drawable-size (get-window sys))
		       :image-array-layers 1
		       :image-usage '(:color-attachment :transfer-src)
		       :image-sharing-mode :exclusive
		       :pre-transform (get-pre-transform sys)
		       :composite-alpha (get-composite-alpha sys)
		       :present-mode (get-present-mode)
		       :clipped t))
	 (swapchain (vk:create-swapchain-khr (get-device sys) create-info))
	 (images (vk:get-swapchain-images-khr (get-device sys) swapchain))
	 (image-views (create-image-view sys images (vk:format select-format))))
    (progn
      (set-swapchain-format sys (vk:format select-format))
      (set-swapchain sys swapchain)
      (set-swapchain-images sys images)
      (set-swapchain-image-views sys image-views))
    (we.dbg:msg :app "~2tcreate swapchain [~a]~%" (get-swapchain sys))
    (we.dbg:msg :app "~2tcreate swapchain format [~a]~%" (get-swapchain-format sys))
    (we.dbg:msg :app "~2tswapchain images [~a]~%" (get-swapchain-images sys))
    (we.dbg:msg :app "~2tswapchain image views [~a]~%" (get-swapchain-image-views sys))))

(defun destroy-swapchain (sys)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (we.dbg:msg :app "destroy swapchain: ->~%")
  (when (and (get-swapchain sys) (get-device sys))
    (we.dbg:msg :app "~2twating device[~a]~%" (get-device sys))
    (vk:device-wait-idle (get-device sys))
    (when (get-swapchain-images sys)
      (mapcar (lambda (image-view)
	   (we.dbg:msg :app "~2tdestroy swapchain imageview [~a]~%" image-view)
	   (vk:destroy-image-view (get-device sys) image-view))
	 (get-swapchain-image-views sys)))
    (we.dbg:msg :app "~2tdestrot swapchain [~a]~%" (get-swapchain sys))
    (vk:destroy-swapchain-khr (get-device sys) (get-swapchain sys))
    (progn
      (set-swapchain sys nil)
      (set-swapchain-format sys nil)
      (set-swapchain-images sys nil)
      (set-swapchain-image-views sys nil))))

(defun vk->init-swapchain (sys format)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (create-swapchain sys format))

(defun vk->destroy-swapchain (sys)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (destroy-swapchain sys))

(defun present (sys cmd)
  ;; use the first present queue
  (let* ((image-index (check-result #'vk:acquire-next-image-khr
				    (get-device sys)
				    (get-swapchain sys)
				    #xffffffff
				    (get-present-complete sys)))
	 (fence (nth image-index (get-fences sys))))
    (vk:wait-for-fences (get-device sys) (list fence) t #xffffffff)
    (vk:reset-fences (get-device sys) (list fence))
    
    (let ((submit-infos (list (vk:make-submit-info
			       :wait-semaphores (list (get-present-complete sys))
			       :wait-dst-stage-mask '(:color-attachment-output)
			       :command-buffers (list cmd)
			       :signal-semaphores (list (get-render-complete sys)))))
	  (create-info (vk:make-present-info-khr
			:wait-semaphores (list (get-render-complete sys))
			:swapchains (list (get-swapchain sys))
			:image-indices (list image-index))))
      (check-result #'vk:queue-submit (get-device-gqueue sys) submit-infos fence)
      (vk:queue-wait-idle (get-device-gqueue sys))
      (vk:queue-present-khr (get-device-pqueue sys) create-info))))

(defmacro with-present ((sys cmd image-index) &body body)
  `(let ((fence (nth (get-current-gcmd-index ,sys) (get-fences ,sys))))
     (vk:wait-for-fences (get-device ,sys) (list fence) t #xffffffff)
     (let ((,image-index (check-result #'vk:acquire-next-image-khr
				      (get-device ,sys)
				      (get-swapchain ,sys)
				      #xffffffff
				      (get-present-complete ,sys))))
       (vk:reset-fences (get-device ,sys) (list fence))
       (progn ,@body)
       (let ((submit-infos (list (vk:make-submit-info
				  :wait-semaphores (list (get-present-complete ,sys))
				  :wait-dst-stage-mask '(:color-attachment-output)
				  :command-buffers (list ,cmd)
				  :signal-semaphores (list (get-render-complete ,sys)))))
	     (create-info (vk:make-present-info-khr
			   :wait-semaphores (list (get-render-complete ,sys))
			   :swapchains (list (get-swapchain ,sys))
			   :image-indices (list ,image-index))))
	 (check-result #'vk:queue-submit (get-device-gqueue ,sys) submit-infos fence)
	 (vk:queue-wait-idle (get-device-gqueue ,sys))
	 (vk:queue-present-khr (get-device-pqueue ,sys) create-info)))))
