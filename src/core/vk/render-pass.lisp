(in-package :%wild-engine.core.vulkan)

(defun attachments (app format)
  "attachments for render pass"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (list
   (vk:make-attachment-description
    :format format
    :samples :1
    :load-op :clear
    :store-op :store
    :stencil-load-op :dont-care
    :stencil-store-op :dont-care
    :initial-layout :undefined
    :final-layout :present-src-khr)
   (vk:make-attachment-description
    :format (find-format app)
    :samples :1
    :load-op :clear
    :store-op :dont-care
    :stencil-load-op :dont-care
    :stencil-store-op :dont-care
    :initial-layout :undefined
    :final-layout :depth-stencil-attachment-optimal)))

(defun subpasses ()
  "subpass for render pass"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (list
   (vk:make-subpass-description
    :pipeline-bind-point :graphics
    :input-attachments nil
    :resolve-attachments nil
    :preserve-attachments nil
    :color-attachments (list
			(vk:make-attachment-reference
			 :attachment 0
			 :layout :color-attachment-optimal))
    :depth-stencil-attachment (list
			       (vk:make-attachment-reference
				:attachment 1
				:layout :depth-stencil-attachment-optimal)))))

(defun dependencies ()
  "dependencies for render pass"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (list
   (vk:make-subpass-dependency
    :src-subpass vk:+subpass-external+
    :dst-subpass 0
    :src-stage-mask '(:color-attachment-output :early-fragment-tests)
    :src-access-mask 0
    :dst-stage-mask '(:color-attachment-output :early-fragment-tests)
    :dst-access-mask '(:color-attachment-write :depth-stencil-attachment-write)
    :dependency-flags :by-region)))

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.render-pass) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(device (%we.utils:device chandle))
					(format (%we.utils:swapchain-format chandle)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "create render-pass : ->~%")
  (let* ((create-info (vk:make-render-pass-create-info
		       :dependencies (dependencies)
		       :subpasses (subpasses)
		       :attachments (attachments app format)))
	 (render-pass (check-result #'vk:create-render-pass device create-info)))
    (setf (%we.utils:render-pass chandle) render-pass)
    (create-depth-buffer app)
    (%we.dbg:msg :app "~2tcreate render pass [~a]~%" render-pass)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.render-pass)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (device (%we.utils:device chandle))
					    (render-pass (%we.utils:render-pass chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy render-pass: ->~%")
  (destroy-depth-buffer app)
  (when render-pass
    (%we.dbg:msg :app "destroy render-pass [~a]~%" render-pass)
    (vk:destroy-render-pass device render-pass)))
