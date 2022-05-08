(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.cmds) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create cmds : ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.cmds))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy cmds: ->~%"))
