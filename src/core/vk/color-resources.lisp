(in-package :%wild-engine.core.vk)

(defun vk->init-color-resources (sys w h
				 &aux
				   (format (get-swapchain-format sys)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "init color resources: -> ~%")
  (let* ((image-create-info (vk:make-image-create-info
			     :image-type :2d
			     :extent (vk:make-extent-3d
				      :width w 
				      :height h
				      :depth 1)
			     :mip-levels 1
			     :array-layers 1
			     :format format
			     :tiling :optimal
			     :initial-layout :undefined
			     :usage '(:transient-attachment :color-attachment)
			     :samples (get-gpu-sample-count sys)
			     :sharing-mode :exclusive))
	 (image (check-result #'vk:create-image (get-device sys) image-create-info))
	 (req-info (vk:get-image-memory-requirements (get-device sys) image))
	 (alloc-info (vk:make-memory-allocate-info
		      :allocation-size (vk:size req-info)
		      :memory-type-index (find-memory sys
						      (vk:memory-type-bits req-info)
						      :device-local)))
	 (mem (funcall (lambda (info)
			 (let ((mem (check-result #'vk:allocate-memory (get-device sys) info)))
			   (vk:bind-image-memory (get-device sys) image mem 0)
			   mem))
		       alloc-info))
	 (image-view (%create-image-view sys image format :color 1)))
    (progn
      (set-color-image sys image)
      (set-color-memory sys mem)
      (set-color-image-view sys image-view))
    (we.dbg:msg :app "~2tcreate color image ~a~%" (get-color-image sys))
    (we.dbg:msg :app "~2tcreate color image view ~a~%" (get-color-image-view sys))
    (we.dbg:msg :app "~2tcreate color mem ~a~%" (get-color-memory sys))))

(defun vk->destroy-color-resources (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "destroy color resources: -> ~%")
  (when (get-color-image-view sys)
    (we.dbg:msg :app "~2tdestroy color image-view ~a~%" (get-color-image-view sys)) 
    (vk:destroy-image-view (get-device sys) (get-color-image-view sys)))
  (when (get-color-image sys)
    (we.dbg:msg :app "~2tdestroy color image ~a~%" (get-color-image sys))
    (vk:destroy-image (get-device sys) (get-color-image sys)))
  (when (get-color-memory sys)
    (we.dbg:msg :app "~2tfree color memory ~a~%" (get-color-memory sys)) 
    (vk:free-memory (get-device sys) (get-color-memory sys)))
  (progn
    (set-color-image sys nil)
    (set-color-image-view sys nil)
    (set-color-memory sys nil)))

