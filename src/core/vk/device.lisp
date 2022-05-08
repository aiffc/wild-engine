(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.device) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create device : ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.device))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy device: ->~%"))
