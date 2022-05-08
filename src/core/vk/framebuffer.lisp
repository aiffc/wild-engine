(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.framebuffer) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create framebuffer : ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.framebuffer))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy framebuffer: ->~%"))
