(in-package :%wild-engine.core.vulkan)

(defparameter *texture-hash* (make-hash-table))

(defmacro with-soil-image ((path data width height &optional (format :rgb)) &body body)
  "macro used to load image with soil"
  `(multiple-value-bind (,data ,width ,height)
       (cl-soil:load-image ,path ,format)
     (declare (fixnum ,width ,height))
     (progn ,@body)
     (cl-soil:free-image-data ,data)))

(defun buffer->image (app buffer image w h)
  "function use to copy buffer to image"
  (declare (optimize  (speed 3) (debug 0) (safety 0)))
  (let ((copy-info (vk:make-buffer-image-copy
		    :buffer-offset 0
		    :buffer-row-length 0
		    :buffer-image-height 0
		    :image-subresource (vk:make-image-subresource-layers
					:aspect-mask :color
					:mip-level 0
					:base-array-layer 0
					:layer-count 1)
		    :image-offset (vk:make-offset-3d)
		    :image-extent (vk:make-extent-3d :width w
						     :height h
						     :depth 1))))
    (with-transfer-cmd (app cmd)
      (vk:cmd-copy-buffer-to-image cmd buffer image :transfer-dst-optimal (list copy-info)))))

(defun create-texture-sampler (app mip-level
			       &aux
				 (chandle (%we.utils:app-handle app))
				 (properties (%we.utils:gpu-properties chandle))
				 (device (%we.utils:device chandle)))
  "function used to create a texture sampler, need to check device feature?"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((create-info (vk:make-sampler-create-info
		       :mag-filter :linear
		       :min-filter :linear
		       :address-mode-u :repeat
		       :address-mode-v :repeat
		       :address-mode-w :repeat
		       :anisotropy-enable t
		       :max-anisotropy (vk:max-sampler-anisotropy (vk:limits properties))
		       :border-color :int-opaque-black
		       :unnormalized-coordinates nil
		       :compare-enable nil
		       :compare-op :always
		       :mipmap-mode :linear
		       :min-lod 0.0
		       :max-lod (we.u:to-single-float mip-level)
		       :mip-lod-bias 0.0))
	 (sampler (check-result #'vk:create-sampler device create-info)))
    sampler))

(defun gen-mipmaps (app image w h &optional (mip-level 1))
  "function use to generate mipmaps"
  (let* ((mip-width w)
	 (mip-height h)
	 (subresource-range (vk:make-image-subresource-range
			     :aspect-mask :color
			     :base-array-layer 0
			     :layer-count 1
			     :level-count 1))
	 (barrier (vk:make-image-memory-barrier
		   :image image
		   :src-queue-family-index %vk:+queue-family-ignored+
		   :dst-queue-family-index %vk:+queue-family-ignored+
		   :subresource-range subresource-range)))
    (with-transfer-cmd (app cmd)
      (loop :for i :from 1 :below mip-level
	    :for src-offset0 := (vk:make-offset-3d)
	    :for src-offset1 := (vk:make-offset-3d :x mip-width :y mip-height :z 1)
	    :for hmip-width := (if (> mip-width 1) (floor (/ mip-width 2.0)) 1) 
	    :for hmip-height := (if (> mip-height 1) (floor (/ mip-height 2.0)) 1) 
	    :for dst-offset0 := (vk:make-offset-3d)
	    :for dst-offset1 := (vk:make-offset-3d :x hmip-width :y hmip-height :z 1)
	    :for blit := (vk:make-image-blit
			  :src-offsets (vector src-offset0 src-offset1)
			  :src-subresource (vk:make-image-subresource-layers
					    :aspect-mask :color
					    :mip-level (- i 1)
					    :base-array-layer 0
					    :layer-count 1)
			  :dst-offsets (vector dst-offset0 dst-offset1)
			  :dst-subresource (vk:make-image-subresource-layers
					    :aspect-mask :color
					    :mip-level i
					    :base-array-layer 0
					    :layer-count 1))
	    :do (progn
		  (setf (vk:old-layout barrier) :transfer-dst-optimal
			(vk:new-layout barrier) :transfer-src-optimal
			(vk:src-access-mask barrier) :transfer-write
			(vk:dst-access-mask barrier) :transfer-read)
		  (vk:cmd-pipeline-barrier cmd nil nil (list barrier) '(:transfer) '(:transfer))
		  (vk:cmd-blit-image cmd
				     image :transfer-src-optimal
				     image :transfer-dst-optimal
				     (list blit)
				     :linear)
		  (setf (vk:old-layout barrier) :transfer-src-optimal
			(vk:new-layout barrier) :shader-read-only-optimal
			(vk:src-access-mask barrier) :transfer-read
			(vk:dst-access-mask barrier) :shader-read)
		  (vk:cmd-pipeline-barrier cmd nil nil (list barrier) '(:transfer) '(:fragment-shader))
		  (setf mip-width hmip-width
			mip-height hmip-height)))
      (setf (vk:base-mip-level (vk:subresource-range barrier)) (1- mip-level)
	    (vk:old-layout barrier) :transfer-dst-optimal
	    (vk:new-layout barrier) :shader-read-only-optimal
	    (vk:src-access-mask barrier) :transfer-write
	    (vk:dst-access-mask barrier) :shader-read)
      (vk:cmd-pipeline-barrier cmd nil nil (list barrier) '(:transfer) '(:fragment-shader)))))

(defun transition-image-layout (app image mip-level old new)
  "function use to transfer image layout"
  (declare (optimize  (speed 3) (debug 0) (safety 0)))
  (let ((barrary (vk:make-image-memory-barrier
			       :old-layout old
			       :new-layout new
			       :src-queue-family-index %vk:+queue-family-ignored+
			       :dst-queue-family-index %vk:+queue-family-ignored+
			       :image image
			       :subresource-range (vk:make-image-subresource-range
						   :aspect-mask :color
						   :base-mip-level 0
						   :level-count mip-level
						   :base-array-layer 0
						   :layer-count 1))))
    (with-transfer-cmd (app cmd)
      (cond ((and (eql new :transfer-dst-optimal)
		  (eql old :undefined))
	     (progn
	       (setf (vk:src-access-mask barrary) 0
		     (vk:dst-access-mask barrary) :transfer-write)
	       (vk:cmd-pipeline-barrier cmd nil nil (list barrary) '(:top-of-pipe) '(:transfer))))
	    ((and (eql new :shader-read-only-optimal)
		  (eql old :transfer-dst-optimal))
	     (progn
	       (setf (vk:src-access-mask barrary) :transfer-write
		     (vk:dst-access-mask barrary) :shader-read)
	       (vk:cmd-pipeline-barrier cmd nil nil (list barrary) '(:transfer) '(:fragment-shader))))
	    (t (format t "unknow~%"))))))

(defun create-image (app name sbuffer width height info-fun
		     &aux
		       (chandle (%we.utils:app-handle app))
		       (device (%we.utils:device chandle))
		       (mem-info (%we.utils:gpu-memory-infos chandle)))
  "function use to create a vk image handle"
  (declare (optimize (speed 1) (safety 0) (debug 0))
	   (integer width height))
  (multiple-value-bind (create-info mem-properties) (funcall info-fun width height)
    (let* ((image (check-result #'vk:create-image device create-info))
	   (req-info (vk:get-image-memory-requirements device image))
	   (alloc-info (vk:make-memory-allocate-info
			:allocation-size (vk:size req-info)
			:memory-type-index (find-memory (vk:memory-type-bits req-info)
							mem-properties
							mem-info)))
	   (mem (funcall (lambda (info)
			   (let ((mem (check-result #'vk:allocate-memory device info)))
			     (vk:bind-image-memory device image mem 0)
			     mem))
			 alloc-info))
	   (mip-level (vk:mip-levels create-info))
	   (image-view (%create-image-view device image (vk:format create-info) :color mip-level))
	   (sampler (create-texture-sampler app mip-level)))
      (push (list :name name
		  :texture image
		  :memory mem
		  :image-view image-view
		  :sampler sampler)
	    (gethash app *texture-hash*))
      (transition-image-layout app image mip-level :undefined :transfer-dst-optimal)
      (buffer->image app sbuffer image width height)
      (gen-mipmaps app image width height mip-level)
      image)))

(defun create-texture (app name path info-fun &optional (format :rgb))
  "function used to create a image from path"
  (with-soil-image (path data width height format)
    (let* ((format-size (if (eql format :rgb) 3 4))
	   (image-size (* width height format-size)))
      (with-stage-buffer (sbuffer smemory app image-size :transfer-src '(:host-visible :host-coherent))
	(map-memory app smemory data image-size)
	(let ((image (create-image app name sbuffer width height info-fun)))
	  (free-memory app smemory)
	  (destroy-buffer app sbuffer)
	  image)))))

(defun destroy-texture (app
			&aux
			  (chandle (%we.utils:app-handle app))
			  (device (%we.utils:device chandle))
			  (texture-info (gethash app *texture-hash*)))
  "function used to destroy all texture with app"
  (declare (optimize  (speed 3) (debug 0) (safety 0)))
  (when texture-info
    (mapcar (lambda (mts)
	 (let ((texture (getf mts :texture))
	       (memory (getf mts :memory))
	       (image-view (getf mts :image-view))
	       (sampler (getf mts :sampler)))
	   (when (and sampler (not (cffi:null-pointer-p sampler)))
	     (%we.dbg:msg :app "destroy sampler ~a~%" sampler)
	     (vk:destroy-sampler device sampler))
	   (when (and image-view (not (cffi:null-pointer-p image-view)))
	     (%we.dbg:msg :app "destroy imageview ~a~%" image-view)
	     (vk:destroy-image-view device image-view))
	   (when (and memory (not (cffi:null-pointer-p memory)))
	     (%we.dbg:msg :app "free memory ~a~%" memory)
	     (vk:free-memory device memory))
	   (when (and texture (not (cffi:null-pointer-p texture)))
	     (%we.dbg:msg :app "destroy texture ~a~%" texture)
	     (vk:destroy-image device texture))))
       texture-info)
    (setf (gethash app *texture-hash*) nil)))
