(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.swapchain) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create swapchain : ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.swapchain))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy swapchain: ->~%"))
