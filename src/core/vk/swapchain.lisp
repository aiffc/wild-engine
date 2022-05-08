(in-package :%wild-engine.core.vulkan)

(defun get-present-mode (modes)
  "not support select present-mode now ready to do"
  (declare (ignore modes))
  :fifo-khr)

(defun get-min-image-count (capabilities)
  (let* ((max-image-count (vk:max-image-count capabilities))
	 (min-image-count (vk:min-image-count capabilities))
	 (image-count (1+ min-image-count)))
    (if (and (> max-image-count 0)
	     (> image-count max-image-count))
	max-image-count
	image-count)))

(defun get-format (formats format)
  (let* ((f (find format formats :key #'vk:format)))
    (if f
	f
	(progn
	  (warn "not support format you selected use the default format~%")
	  (first formats)))))

(defun get-composite-alpha (capabilities)
  (let ((default :inherit)
	(composite-alphas (vk:supported-composite-alpha capabilities)))
    (if (member :opaque composite-alphas)
	:opaque
	default)))

(defun get-pre-transform (capabilities)
  (let ((current-transform (vk:current-transform capabilities))
	(supported-transform (vk:supported-transforms capabilities)))
    (if (member :identity supported-transform)
	:identity
	current-transform)))

(defun get-image-extent (window)
  (sdl-vulkan:sdl-get-drawable-size window))

(defun %create-image-view (device image format aspect mip-level)
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
	 (image-view (check-result #'vk:create-image-view device create-info)))
    image-view))

(defun create-image-view (device images format)
  "function used to create imageviews"
  (mapcar (lambda (image)
       (%create-image-view device image format :color 1))
     images))

(defun create-swapchain (app format
			 &aux
			   (chandle (%we.utils:app-handle app))
			   (device (%we.utils:device chandle))
			   (present-modes (%we.utils:gpu-present-mode chandle))
			   (capabilities (%we.utils:gpu-capabilities chandle))
			   (formats (%we.utils:gpu-formats chandle))
			   (surface (%we.utils:surface chandle))
			   (window (%we.utils:window chandle)))
  "function used to create a swapchain"
  (let* ((select-format (get-format formats format))
	 (create-info (vk:make-swapchain-create-info-khr
		       :surface surface
		       :min-image-count (get-min-image-count capabilities)
		       :image-format (vk:format select-format)
		       :image-color-space (vk:color-space select-format)
		       :image-extent (get-image-extent window)
		       :image-array-layers 1
		       :image-usage '(:color-attachment :transfer-src)
		       :image-sharing-mode :exclusive
		       :pre-transform (get-pre-transform capabilities)
		       :composite-alpha (get-composite-alpha capabilities)
		       :present-mode (get-present-mode present-modes)
		       :clipped t))
	 (swapchain (vk:create-swapchain-khr device create-info))
	 (images (vk:get-swapchain-images-khr device swapchain))
	 (image-views (create-image-view device images (vk:format select-format))))
    (%we.dbg:msg :app "~2tcreate swapchain [~a]~%" swapchain)
    (%we.dbg:msg :app "~2tcreate swapchain format [~a]~%" (vk:format select-format))
    (%we.dbg:msg :app "~2tswapchain images [~a]~%" images)
    (%we.dbg:msg :app "~2tswapchain image views [~a]~%" image-views)
    (setf (%we.utils:swapchain chandle) swapchain
	  (%we.utils:swapchain-format chandle) (vk:format select-format)
	  (%we.utils:swapchain-images chandle) images
	  (%we.utils:swapchain-image-views chandle) image-views)))

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.swapchain) args)
  (declare (ignore handle))
  (%we.dbg:msg :app "create swapchain : ->~%")
  (let ((swapchain-format (getf args :sformat)))
    (create-swapchain app swapchain-format)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.swapchain)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (swapchain (%we.utils:swapchain chandle))
					    (device (%we.utils:device chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy swapchain: ->~%")
  (when (and swapchain device)
    (%we.dbg:msg :app "~2twating device[~a]~%" device)
    (vk:device-wait-idle device)
    (%we.dbg:msg :app "~2tdestrot swapchain [~a]~%" swapchain)
    (vk:destroy-swapchain-khr device swapchain)))

(defun present (app index
		&aux
		  (chandle (%we.utils:app-handle app))
		  (device (%we.utils:device chandle))
		  (image-available (%we.utils:signal-image-available chandle))
		  (render-finish (%we.utils:signal-render-finish chandle))
		  (swapchain (%we.utils:swapchain chandle))
		  (cmd (nth index (%we.utils:cmds-graphics chandle)))
		  (gqueue (nth 0 (%we.utils:device-gqueues chandle)))  ;; use the first graphics queue
		  (pqueue (nth 0 (%we.utils:device-pqueues chandle)))) ;; use the first present queue
  (let* ((image-index (check-result #'vk:acquire-next-image-khr
				    device
				    swapchain
				    #xffffffff
				    (first image-available)))
	 (submit-infos (list (vk:make-submit-info
			      :wait-semaphores image-available
			      :wait-dst-stage-mask '(:color-attachment-output)
			      :command-buffers (list cmd)
			      :signal-semaphores render-finish)))
	 (create-info (vk:make-present-info-khr
		       :wait-semaphores render-finish
		       :swapchains (list swapchain)
		       :image-indices (list image-index))))
    (check-result #'vk:queue-submit gqueue submit-infos)
    (vk:queue-wait-idle gqueue)
    (vk:queue-present-khr pqueue create-info)))
