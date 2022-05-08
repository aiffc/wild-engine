(in-package :%wild-engine.core.vulkan)

(defparameter *depth-hash* (make-hash-table))

(define-condition not-support-depth-format (error)
  ()
  (:report (lambda (obj stream)
	     (declare (ignore obj))
	     (format stream "device not support depth buffer~%"))))

(defun get-depth-image-view (app)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (getf (gethash app *depth-hash*) :image-view))

(defun find-support-format (app candidates tiling features
			    &aux
			      (chandle (%we.utils:app-handle app))
			      (gpu (%we.utils:gpu chandle)))
  (loop :for format :in candidates
	:for properties := (vk:get-physical-device-format-properties gpu format)
	:do (when (or (and (eql tiling :linear)
			   (member features (vk:linear-tiling-features properties)))
		      (and (eql tiling :optimal)
			   (member features (vk:optimal-tiling-features properties)))) 
	      (return-from find-support-format format)))
  (error 'not-support-depth-format))

(defun find-format (app)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (find-support-format app
		       '(:d32-sfloat :d32-sfloat-s8-uint :d24-unorm-s8-uint)
		       :optimal
		       :depth-stencil-attachment))


(defun create-depth-image (app device width height format mem-info)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((image-create-info (vk:make-image-create-info
			     :image-type :2d
			     :extent (vk:make-extent-3d
				      :width width
				      :height height
				      :depth 1)
			     :mip-levels 1
			     :array-layers 1
			     :format format
			     :tiling :optimal
			     :initial-layout :undefined
			     :usage :depth-stencil-attachment
			     :samples :1
			     :sharing-mode :exclusive))
	 (image (check-result #'vk:create-image device image-create-info))
	 (req-info (vk:get-image-memory-requirements device image))
	 (alloc-info (vk:make-memory-allocate-info
		      :allocation-size (vk:size req-info)
		      :memory-type-index (find-memory (vk:memory-type-bits req-info)
						      :device-local
						      mem-info)))
	 (mem (funcall (lambda (info)
			 (let ((mem (check-result #'vk:allocate-memory device info)))
			   (vk:bind-image-memory device image mem 0)
			   mem))
		       alloc-info))
	 (image-view (%create-image-view device image format :depth 1)))
    (%we.dbg:msg :app "~2tcreate depth image ~a~%" image)
    (%we.dbg:msg :app "~2tcreate depth image-view ~a~%" image-view)
    (%we.dbg:msg :app "~2tcreate depth mem ~a~%" mem) 
    (setf (gethash app *depth-hash*)
	  (list :images image
		:memory mem
		:image-view image-view))))

(defun create-depth-buffer (app
			    &aux
			      (chandle (%we.utils:app-handle app))
			      (device (%we.utils:device chandle))
			      (window (%we.utils:window chandle))
			      (mem-info (%we.utils:gpu-memory-infos chandle)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((depth-format (find-format app))
	 (window-size (sdl-vulkan:sdl-get-drawable-size window))
	 (width (vk:width window-size))
	 (height (vk:height window-size)))
    (create-depth-image app device width height depth-format mem-info)))

(defun destroy-depth-buffer (app
			     &aux
			       (chandle (%we.utils:app-handle app))
			       (device (%we.utils:device chandle))
			       (depths (gethash app *depth-hash*))
			       (image (getf depths :image))
			       (memory (getf depths :memory))
			       (image-view (getf depths :image-view)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (when depths
    (when image-view
      (%we.dbg:msg :app "~2tdestroy depth image-view ~a~%" image-view) 
      (vk:destroy-image-view device image-view))
    (when image
      (%we.dbg:msg :app "~2tdestroy depth image ~a~%" image)
      (vk:destroy-image device image))
    (when memory
      (%we.dbg:msg :app "~2tfree depth memory ~a~%" memory) 
      (vk:free-memory device memory))))
