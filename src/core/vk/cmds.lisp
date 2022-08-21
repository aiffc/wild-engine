(in-package :%wild-engine.core.vk)

(defun vk->alloc-cmds (sys)
  (we.dbg:msg :app "create cmds : ->~%")
  (progn
    (set-cmds-graphics sys
		       (check-result #'vk:allocate-command-buffers
				     (get-device sys)
				     (vk:make-command-buffer-allocate-info
				      :command-pool (get-cmd-pool-graphics sys)
				      :level :primary
				      :command-buffer-count (length (get-swapchain-images sys)))))
    (set-cmds-compute sys
		      (check-result #'vk:allocate-command-buffers
				    (get-device sys)
				    (vk:make-command-buffer-allocate-info
				     :command-pool (get-cmd-pool-compute sys)
				     :level :primary
				     :command-buffer-count 1))))
  (we.dbg:msg :app "~2tallocate graphics cmds [~a] in pool [~a]~%" (get-cmds-graphics sys) (get-cmd-pool-graphics sys))
  (we.dbg:msg :app "~2tallocate compute cmds [~a] in pool [~a]~%" (get-cmds-compute sys) (get-cmd-pool-compute sys)))

(defun vk->free-cmds (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (we.dbg:msg :app "destroy cmds: ->~%")
  (when (and (get-device sys) (get-cmd-pool-graphics sys) (get-cmds-graphics sys))
    (we.dbg:msg :app "~2tfree graphics cmds ~a~%" (get-cmds-graphics sys))
    (vk:free-command-buffers (get-device sys) (get-cmd-pool-graphics sys) (get-cmds-graphics sys))
    (set-cmds-graphics sys nil))
  (when (and (get-device sys) (get-cmd-pool-compute sys) (get-cmds-compute sys))
    (we.dbg:msg :app "~2tfree compute cmds ~a~%" (get-cmds-compute sys))
    (vk:free-command-buffers (get-device sys) (get-cmd-pool-compute sys) (get-cmds-compute sys))
    (set-cmds-compute sys nil)))



;; (defmacro %with-gcmd ((index) &body body)
;;   `(progn
;;      (begin-gcmd ,index)
;;      ,@body
;;      (end-gcmd ,index)))

;; (defmacro %with-render-pass ((index x y width height clear-color) &body body)
;;   `(progn
;;      (begin-render-pass ,index ,x ,y ,width ,height ,clear-color)
;;      ,@body
;;      (end-render-pass ,index)))

;; (defmacro with-gcmd ((cmd x y width height clear-color)
;; 		     &body body)
;;   `(dotimes (index (length (get-cmds-graphics sys)))
;;      (let ((,cmd (nth index (get-cmds-graphics sys))))
;;        (%with-gcmd (index)
;; 	 (%with-render-pass (index ,x ,y ,width ,height ,clear-color)
;; 	 ,@body)))
;;      (present index)))

(defun begin-gcmd (cmd)
  "begin command buffer [index]"
  (we.dbg:msg :app-high "~%enter begin cmd [~a]->~%" cmd)
  (let ((begin-info (vk:make-command-buffer-begin-info)))
    (check-result #'vk:begin-command-buffer cmd begin-info)))

(defun end-gcmd (cmd)
  "end command buffer [index]"
  (we.dbg:msg :app-high "~%enter end cmd [~a]->~%" cmd)
  (check-result #'vk:end-command-buffer cmd))

(defmacro %with-gcmd ((cmd) &body body)
  `(progn
     (begin-gcmd ,cmd)
     ,@body
     (end-gcmd ,cmd)))

(defun begin-render-pass (cmd sys image-index x y width height clear-color
			  &aux
			    (framebuffers (get-frame-buffers sys))
			    (framebuffer (nth image-index framebuffers))
			    (render-pass (get-render-pass sys)))
  "begin render pass set render area and background"
  (we.dbg:msg :app-high "~%enter begin renderpass [~a]->~%" render-pass)
  (let* ((offset (vk:make-offset-2d :x x :y y))
	 (extent (vk:make-extent-2d :width width :height height))
	 (render-area (vk:make-rect-2d
		       :offset offset
		       :extent extent))
	 (clear-color (list
		       (vk:make-clear-color-value :float-32 clear-color)
		       (vk:make-clear-value :depth-stencil
					    (vk:make-clear-depth-stencil-value :depth 1.0))))
	 (create-info (vk:make-render-pass-begin-info
		       :render-pass render-pass
		       :framebuffer framebuffer
		       :render-area render-area
		       :clear-values clear-color)))
    (vk:cmd-begin-render-pass cmd create-info :inline)))

(defun end-render-pass (cmd sys
			&aux
			  (render-pass (get-render-pass sys)))
  "end render pass"
  (we.dbg:msg :app-high "~%enter end renderpass [~a]->~%" render-pass)
  (vk:cmd-end-render-pass cmd))

(defmacro %with-render-pass ((cmd sys image-index x y width height clear-color) &body body)
  `(progn
     (begin-render-pass ,cmd ,sys ,image-index ,x ,y ,width ,height ,clear-color)
     ,@body
     (end-render-pass ,cmd ,sys)))

(defun update-gcmd-index (sys
			  &aux
			    (cmd-size (length (get-cmds-graphics sys)))
			    (index (get-current-gcmd-index sys)))
  (declare (type integer cmd-size index))
  (if (= 0 (mod index cmd-size))
      (set-current-gcmd-index sys 0)
      (set-current-gcmd-index sys (1+ index))))

(defmacro with-gcmd ((cmd sys x y width height clear-color) &body body)
  `(progn
     (let ((,cmd (nth (get-current-gcmd-index ,sys) (get-cmds-graphics ,sys))))
       (vk:reset-command-buffer ,cmd)
       (with-present (,sys ,cmd image-index)
	 (%with-gcmd (,cmd)
	   (%with-render-pass (,cmd ,sys image-index ,x ,y ,width ,height ,clear-color)
	     ,@body)))
       (update-gcmd-index ,sys))))
