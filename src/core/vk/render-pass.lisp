(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.render-pass) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "create render-pass : ->~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.render-pass))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy render-pass: ->~%"))
