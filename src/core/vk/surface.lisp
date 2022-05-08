(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.surface) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create surface: ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.surface))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy surface: ->~%"))
