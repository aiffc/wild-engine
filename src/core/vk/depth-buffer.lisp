(in-package :%wild-engine.core.vk)

(define-condition not-support-depth-format (error)
  ()
  (:report (lambda (obj stream)
	     (declare (ignore obj))
	     (format stream "(get-device sys) not support depth buffer~%"))))

(defun find-support-format (sys candidates tiling features)
  (loop :for format :in candidates
	:for properties := (vk:get-physical-device-format-properties (get-gpu sys) format)
	:do (when (or (and (eql tiling :linear)
			   (member features (vk:linear-tiling-features properties)))
		      (and (eql tiling :optimal)
			   (member features (vk:optimal-tiling-features properties)))) 
	      (return-from find-support-format format)))
  (error 'not-support-depth-format))

(defun find-format (sys)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (find-support-format sys
		       '(:d32-sfloat :d32-sfloat-s8-uint :d24-unorm-s8-uint)
		       :optimal
		       :depth-stencil-attachment))


(defun vk->init-depth-stencil (sys w h)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "init depth stencil: -> ~%")
  (let* ((format (find-format sys))
	 (image-create-info (vk:make-image-create-info
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
			     :usage :depth-stencil-attachment
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
	 (image-view (%create-image-view sys image format :depth 1)))
    (progn
      (set-depth-format sys format)
      (set-depth-image sys image)
      (set-depth-image-view sys image-view)
      (set-depth-memory sys mem))
    (we.dbg:msg :app "~2tcreate depth image ~a~%" (get-depth-image sys))
    (we.dbg:msg :app "~2tcreate depth image-view ~a~%" (get-depth-image-view sys))
    (we.dbg:msg :app "~2tcreate depth mem ~a~%" (get-depth-memory sys))))

(defun vk->destroy-depth-stencil (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "destroy depth stencil: -> ~%")
  (when (get-depth-image-view sys)
      (we.dbg:msg :app "~2tdestroy depth image-view ~a~%" (get-depth-image-view sys)) 
      (vk:destroy-image-view (get-device sys) (get-depth-image-view sys)))
  (when (get-depth-image sys)
	(we.dbg:msg :app "~2tdestroy depth image ~a~%" (get-depth-image sys))
	(vk:destroy-image (get-device sys) (get-depth-image sys)))
  (when (get-depth-memory sys)
	(we.dbg:msg :app "~2tfree depth memory ~a~%" (get-depth-memory sys)) 
	(vk:free-memory (get-device sys) (get-depth-memory sys)))
  (progn
      (set-depth-format sys nil)
      (set-depth-image sys nil)
      (set-depth-image-view sys nil)
      (set-depth-memory sys nil)))
