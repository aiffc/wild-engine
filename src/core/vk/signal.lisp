(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.signal) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create signal : ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.signal))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy signal: ->~%"))
