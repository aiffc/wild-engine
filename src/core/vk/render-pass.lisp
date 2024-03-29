(in-package :%wild-engine.core.vk)

(defun attachments (sys format anti-aliasing)
  "attachments for render pass"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (list*
   (vk:make-attachment-description
    :format format
    :samples (get-gpu-sample-count sys)
    :load-op :clear
    :store-op :store
    :stencil-load-op :dont-care
    :stencil-store-op :dont-care
    :initial-layout :undefined
    :final-layout (if anti-aliasing :color-attachment-optimal :present-src-khr))
   (vk:make-attachment-description
    :format (get-depth-format sys)
    :samples (get-gpu-sample-count sys)
    :load-op :clear
    :store-op :dont-care
    :stencil-load-op :dont-care
    :stencil-store-op :dont-care
    :initial-layout :undefined
    :final-layout :depth-stencil-attachment-optimal)
   (when anti-aliasing
     (list (vk:make-attachment-description
	    :format format
	    :samples :1
	    :load-op :dont-care
	    :store-op :store
	    :stencil-load-op :dont-care
	    :stencil-store-op :dont-care
	    :initial-layout :undefined
	    :final-layout :present-src-khr)))))

(defun subpasses (anti-aliasing)
  "subpass for render pass"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (list
   (vk:make-subpass-description
    :pipeline-bind-point :graphics
    :input-attachments nil
    :preserve-attachments nil
    :color-attachments (list
			(vk:make-attachment-reference
			 :attachment 0
			 :layout :color-attachment-optimal))
    :depth-stencil-attachment (list
    			       (vk:make-attachment-reference
    				:attachment 1
    				:layout :depth-stencil-attachment-optimal))
    :resolve-attachments (when anti-aliasing
			   (list
			    (vk:make-attachment-reference
			     :attachment 2
			     :layout :color-attachment-optimal))))))

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


(defun vk->init-render-pass (sys anti-aliasing)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "create render-pass : ->~%")
  (let ((create-info (vk:make-render-pass-create-info
		      :dependencies (dependencies)
		      :subpasses (subpasses anti-aliasing)
		      :attachments (attachments sys (get-swapchain-format sys) anti-aliasing))))
    (set-render-pass sys (check-result #'vk:create-render-pass (get-device sys) create-info))
    (we.dbg:msg :app "~2tcreate render pass [~a]~%" (get-render-pass sys))))

(defun vk->destroy-render-pass (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "destroy render-pass: ->~%")
  (when (get-render-pass sys)
    (we.dbg:msg :app "~2tdestroy render-pass [~a]~%" (get-render-pass sys))
    (vk:destroy-render-pass (get-device sys) (get-render-pass sys))
    (set-render-pass sys nil)))
