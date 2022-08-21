(in-package :%wild-engine.core.vk)

(defun vk->init-window (sys w h x y title)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (set-window sys (sdl2:create-window :title title
				      :x x
				      :y y
				      :w w
				      :h h
				      :flags '(:vulkan)))
  (we.dbg:msg :app "create window: ->~%~2t~a~%" (get-window sys)))

(defun vk->destroy-window (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "destroy window: ->~%~2t~a~%" (get-window sys))
  (progn
    (sdl2:destroy-window (get-window sys))
    (set-window sys nil)))
