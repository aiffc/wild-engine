(in-package :%wild-engine.core.vk)

(defstruct texture
  image view sampler memory)

(defun create-image-sampler (sys
			     &aux
			       (device (get-device sys))
			       (properties (get-gpu-properties sys)))
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
		       :max-lod 0.0
		       :mip-lod-bias 0.0))
	 (sampler (check-result #'vk:create-sampler device create-info)))
    (we.dbg:msg :app "create image sampler ~a~%" sampler)
    sampler))

(defun create-image (sys width height format
		     &aux
		       (device (get-device sys))
		       (sformat (case format
				  (:rgb :r8g8b8-srgb)
				  (:rgba :r8g8b8a8-srgb))))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((create-info (vk:make-image-create-info
		       :image-type :2d
		       :extent (vk:make-extent-3d
				:width width
				:height height
				:depth 1)
		       :mip-levels 1
		       :array-layers 1
		       :format sformat
		       :tiling :optimal
		       :initial-layout :undefined
		       :usage '(:transfer-dst :sampled)
		       :sharing-mode :exclusive
		       :samples :1))
	 (image (vk:create-image device create-info))
	 (mem-req (vk:get-image-memory-requirements device image))
	 (alloc-info (vk:make-memory-allocate-info
		    :allocation-size (vk:size mem-req)
		    :memory-type-index (find-memory sys
						    (vk:memory-type-bits mem-req)
						    :device-local)))
	 (mem (vk:allocate-memory device alloc-info))
	 (image-sampler (create-image-sampler sys)))
    (we.dbg:msg :app "create image ~a~%" image)
    (vk:bind-image-memory device image mem 0)
    (let ((image-view (%create-image-view sys image sformat :color 1)))
      (we.dbg:msg :app "create image view ~a~%" image-view)
      (values (make-texture :image image
			    :view image-view
			    :sampler image-sampler
			    :memory mem)))))

(defun destroy-image (sys texture
		      &aux
			(device (get-device sys))
			(image-sampler (texture-sampler texture))
			(image-view (texture-view texture))
			(image (texture-image texture))
			(memory (texture-memory texture)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (when image-sampler
    (we.dbg:msg :app "destroy image sampler ~a~%" image-sampler)
    (vk:destroy-sampler device image-sampler))
  (when image-view
    (we.dbg:msg :app "destroy image view ~a~%" image-view)
    (vk:destroy-image-view device image-view))
  (when image
    (we.dbg:msg :app "destroy image ~a~%" image)
    (vk:destroy-image device image))
  (when memory
    (we.dbg:msg :app "free image memory ~a~%" memory)
    (vk:free-memory device memory)))

(defun transition-image-layout (sys image old new)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((barrier (vk:make-image-memory-barrier
		  :old-layout old
		  :new-layout new
		  :src-queue-family-index %vk:+queue-family-ignored+
		  :dst-queue-family-index %vk:+queue-family-ignored+
		  :image image
		  :subresource-range (vk:make-image-subresource-range
				      :aspect-mask :color
				      :base-mip-level 0
				      :level-count 1
				      :base-array-layer 0
				      :layer-count 1)
		  :src-access-mask 0
		  :dst-access-mask 0)))
    (with-transfer-cmd (sys cmd)
      (cond ((and (eql new :transfer-dst-optimal)
		  (eql old :undefined))
	     (progn
	       (setf (vk:src-access-mask barrier) 0
		     (vk:dst-access-mask barrier) :transfer-write)
	       (vk:cmd-pipeline-barrier cmd nil nil (list barrier) '(:top-of-pipe) '(:transfer))))
	    ((and (eql new :shader-read-only-optimal)
		  (eql old :transfer-dst-optimal))
	     (progn
	       (setf (vk:src-access-mask barrier) :transfer-write
		     (vk:dst-access-mask barrier) :shader-read)
	       (vk:cmd-pipeline-barrier cmd nil nil (list barrier) '(:transfer) '(:fragment-shader))))
	    (t (format t "unknow~%"))))))

(defun buffer->image (sys buffer image w h)
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
    (with-transfer-cmd (sys cmd)
      (vk:cmd-copy-buffer-to-image cmd buffer image :transfer-dst-optimal (list copy-info)))))


(defmacro deftexture (name path format)
  "usage->
  (deftexture test-texture *texture* :rgba)
usage-export
  createt-*name*
  witht-*name*
"
  
  (let ((create-fun (we.u:create-symbol 'createt- name))
	(with-macro (we.u:create-symbol 'witht- name)))
    `(progn
       (eval-when (:compile-toplevel :execute :load-toplevel))
       (defun ,create-fun (sys)
	 (multiple-value-bind (data width height)
	     (cl-soil:load-image ,path ,format)
	   (declare (fixnum width height))
	   (let ((image-size (* width height ,(if (eql format :rgb) 3 4))))
	     (multiple-value-bind (sbuffer smemory)
		 (we.vk::create-buffer sys image-size :transfer-src '(:host-visible :host-coherent))
	       (we.vk::map-memory sys smemory data image-size)
	       (multiple-value-bind (texture-ptr)
		   (we.vk::create-image sys width height ,format)
		 (transition-image-layout sys (texture-image texture-ptr) :undefined :transfer-dst-optimal)
		 (buffer->image sys sbuffer (texture-image texture-ptr) width height)
		 (transition-image-layout sys (texture-image texture-ptr) :transfer-dst-optimal :shader-read-only-optimal)
		 (we.vk::destroy-buffer sys sbuffer smemory)
		 (cl-soil:free-image-data data)
		 texture-ptr)))))
       (defmacro ,with-macro ((image sys) &body wbody)
	 (let ((create-fun (we.u:create-symbol 'createt- ',name)))
	   `(multiple-value-bind (,image)
		(,create-fun ,sys)
	      (progn ,@wbody)
	      (we.vk::destroy-image ,sys ,image)))))))
