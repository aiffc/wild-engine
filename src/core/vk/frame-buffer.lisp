(in-package :%wild-engine.core.vk)

(defun vk->init-frame-buffer (sys w h anti-aliasing)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "create framebuffer : ->~%")
  (set-frame-buffers sys
		     (mapcar (lambda (iv)
			  (let ((f (vk:create-framebuffer
				    (get-device sys)
				    (vk:make-framebuffer-create-info
				     :render-pass (get-render-pass sys)
				     :attachments (if anti-aliasing
						       (list (get-color-image-view sys)
							     (get-depth-image-view sys)
							     iv)
						       (list iv (get-depth-image-view sys)))
				     :width w
				     :height h
				     :layers 1))))
			    (we.dbg:msg :app "~2tcreate framebuffer [~a]~%" f)
			    f))
			(get-swapchain-image-views sys))))

(defun vk->destroy-frame-buffer (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "destroy framebuffer: ->~%")
  (when (get-frame-buffers sys)
    (mapcar (lambda (fb)
	 (we.dbg:msg :app "~2tdestroy framebuffer [~a]~%" fb)
	 (vk:destroy-framebuffer (get-device sys) fb))
       (get-frame-buffers sys))
    (set-frame-buffers sys nil)))
