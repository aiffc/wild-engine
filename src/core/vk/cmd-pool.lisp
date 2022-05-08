(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.cmd-pool) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create cmd-pool : ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.cmd-pool))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy cmd-pool: ->~%"))
