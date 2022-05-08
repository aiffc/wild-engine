(in-package :%wile-engine.core.windows)

(defun parse-window-arg (args)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (list :title (%we.utils:set-value args :win-title "wild engine window")
	:x (%we.utils:set-value args :win-x :centered)
	:y (%we.utils:set-value args :win-y :centered)
	:w (%we.utils:set-value args :win-w 600)
	:h (%we.utils:set-value args :win-h 600)
	:flags '(:vulkan)))

(defmethod %we.utils:make-app :after (app (handle %we.utils:window) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(window-args (parse-window-arg args))
					(window-handle (apply #'sdl2:create-window window-args)))
  (declare (ignore handle))
  (%we.dbg:msg :app "create window: ->~%~2t~a~%" window-handle)
  (setf (%we.utils:window chandle) window-handle))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:window)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (window-handle (%we.utils:window chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy window: ->~%~2t~a~%" window-handle)
  (sdl2:destroy-window window-handle))
