(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.framebuffer) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(device (%we.utils:device chandle))
					(image-views (%we.utils:swapchain-image-views chandle))
					(render-pass (%we.utils:render-pass chandle))
					(depth-image-view (get-depth-image-view app))
					(window (%we.utils:window chandle))
					(window-size (sdl-vulkan:sdl-get-drawable-size window)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "create framebuffer : ->~%")
  (setf (%we.utils:framebuffer chandle)
	(mapcar (lambda (iv)
	     (let ((f (vk:create-framebuffer
		       device
		       (vk:make-framebuffer-create-info
			:render-pass render-pass
			:attachments (list iv depth-image-view)
			:width (vk:width window-size)
			:height (vk:height window-size)
			:layers 1))))
	       (%we.dbg:msg :app "~2tcreate framebuffer [~a]~%" f)
	       f))
	   image-views)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.framebuffer)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (device (%we.utils:device chandle))
					    (framebuffers (%we.utils:framebuffer chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy framebuffer: ->~%")
  (when framebuffers
    (mapcar (lambda (fb)
	 (%we.dbg:msg :app "~2tdestroy framebuffer [~a]~%" fb)
	 (vk:destroy-framebuffer device fb))
       framebuffers)))
