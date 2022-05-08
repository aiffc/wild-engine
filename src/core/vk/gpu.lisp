(in-package :%wile-engine.core.vulkan)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.gpu) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "pickup gpu: -> ~%"))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.gpu))
  (declare (ignore handle))
  (%we.dbg:msg :app "nothing for destroy gpu~%"))
