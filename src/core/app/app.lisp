(in-package :%wild-engine.core.app)

(defmethod %we.utils:make-app (app (handle %we.utils:app) args)
  "args ->
win-title [window title]
win-x [window position of x]
win-y [window position of y]
win-w [window's width]
win-h [window's height]
pipefun [pipeline initialize list]
"
  (declare (ignore args))
  (setf (gethash app %we.utils:*apps*) handle))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:app))
  (declare (ignore handle))
  (%we.dbg:msg :app "free resource~%")
  (%we.vk:free-buffer app)
  (%we.vk:destroy-texture app)
  (%we.vk:destroy-uniform-buffer app))

(defmethod %we.utils:destroy-app (app (handle %we.utils:app))
  (declare (ignore handle))
  (setf (gethash app %we.utils:*apps*) nil)
  (%we.dbg:msg :app "end destroy app~%"))


