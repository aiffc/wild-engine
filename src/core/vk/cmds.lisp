(in-package :%wild-engine.core.vulkan)

;;(defparameter *current-pipeline* nil)
(defparameter *current-cmd-index* 0)

(defmethod %we.utils:make-app :after (app (handle %we.utils:vk.cmds) args
				      &aux
					(chandle (%we.utils:app-handle app))
					(device (%we.utils:device chandle))
					(cpg (%we.utils:cmd-pool-graphics chandle))
					(cpc (%we.utils:cmd-pool-compute chandle))
					(framebuffers (%we.utils:framebuffer chandle)))
  (declare (ignore handle args))
  (%we.dbg:msg :app "create cmds : ->~%")
  (let ((cbg (check-result #'vk:allocate-command-buffers
			   device
			   (vk:make-command-buffer-allocate-info
			    :command-pool cpg
			    :level :primary
			    :command-buffer-count (length framebuffers))))
	(cbc (check-result #'vk:allocate-command-buffers
			   device
			   (vk:make-command-buffer-allocate-info
			    :command-pool cpc
			    :level :primary
			    :command-buffer-count 1))))
    (%we.dbg:msg :app "~2tallocate graphics cmds [~a] in pool [~a]~%" cbg cpg)
    (%we.dbg:msg :app "~2tallocate compute cmds [~a] in pool [~a]~%" cbc cpc)
    (setf (%we.utils:cmds-graphics chandle) cbg
	  (%we.utils:cmds-compute chandle) cbc)))

(defmethod %we.utils:destroy-app :before (app (handle %we.utils:vk.cmds)
					  &aux
					    (chandle (%we.utils:app-handle app))
					    (device (%we.utils:device chandle))
					    (cpg (%we.utils:cmd-pool-graphics chandle))
					    (cpc (%we.utils:cmd-pool-compute chandle))
					    (cbg (%we.utils:cmds-graphics chandle))
					    (cbc (%we.utils:cmds-compute chandle)))
  (declare (ignore handle))
  (%we.dbg:msg :app "destroy cmds: ->~%")
  (when (and device cpg cbg)
    (%we.dbg:msg :app "~2tfree graphics cmds ~a~%" cbg)
    (vk:free-command-buffers device cpg cbg))
  (when (and device cpc cbc)
    (%we.dbg:msg :app "~2tfree compute cmds ~a~%" cbc)
    (vk:free-command-buffers device cpc cbc)))

(defun begin-gcmd (app index
		   &aux
		     (chandle (%we.utils:app-handle app))
		     (cmd (nth index (%we.utils:cmds-graphics chandle))))
  "begin command buffer [index]"
  (%we.dbg:msg :app-high "~%enter begin cmd [~a]->~%" cmd)
  (let ((begin-info (vk:make-command-buffer-begin-info)))
    (check-result #'vk:begin-command-buffer cmd begin-info)))

(defun end-gcmd (app index
		 &aux
		   (chandle (%we.utils:app-handle app))
		   (cmd (nth index (%we.utils:cmds-graphics chandle))))
  "end command buffer [index]"
  (%we.dbg:msg :app-high "~%enter end cmd [~a]->~%" cmd)
  (check-result #'vk:end-command-buffer cmd))

(defun begin-render-pass (app index x y width height clear-color
			  &aux
			    (chandle (%we.utils:app-handle app))
			    (cmd (nth index (%we.utils:cmds-graphics chandle)))
			    (render-pass (%we.utils:render-pass chandle))
			    (framebuffer (nth index (%we.utils:framebuffer chandle))))
  "begin render pass set render area and background"
  (%we.dbg:msg :app-high "~%enter begin renderpass [~a]->~%" render-pass)
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

(defun end-render-pass (app index
			&aux
			  (chandle (%we.utils:app-handle app))
			  (render-pass (%we.utils:render-pass chandle))
			  (cmd (nth index (%we.utils:cmds-graphics chandle))))
  "end render pass"
  (%we.dbg:msg :app-high "~%enter end renderpass [~a]->~%" render-pass)
  (vk:cmd-end-render-pass cmd))

(defmacro %with-gcmd ((app index) &body body)
  `(progn
     (begin-gcmd ,app ,index)
     ,@body
     (end-gcmd ,app ,index)))

(defmacro %with-render-pass ((app index x y width height clear-color) &body body)
  `(progn
     (begin-render-pass ,app ,index ,x ,y ,width ,height ,clear-color)
     ,@body
     (end-render-pass ,app ,index)))

(defun call-update-funs (app index update-funs)
  (mapc (lambda (fun)
       (funcall fun app index))
     update-funs))

(defun bind-pipeline (app name cmd
		      &aux
			(pipeline (slot-pipeline app name))
			(playout (slot-pipeline app name :layout))
			(sets (slot-pipeline app name :descriptor-sets)))
  (when (and *current-pipeline* pipeline)
    (vk:cmd-bind-pipeline cmd :graphics pipeline)
    (when (and playout sets)
      (vk:cmd-bind-descriptor-sets cmd :graphics
				   playout
				   0
				   (list (nth *current-cmd-index* sets))
				   nil))))

;; (defun set-current-pipeline (name)
;;   (setf *current-pipeline* name))

(defmacro with-gcmd ((app cmd x y width height clear-color update-funs)
		     &body body)
  `(let* ((chandle (%we.utils:app-handle ,app))
	  (cmds (%we.utils:cmds-graphics chandle)))
     (dotimes (index (length cmds))
       (let ((,cmd (nth index cmds)))
	 (call-update-funs ,app index ,update-funs)
	 (setf *current-cmd-index* index)
	 (%with-gcmd (,app index)
	   (%with-render-pass (,app index ,x ,y ,width ,height ,clear-color)
	     ,@body)))
       (present ,app index))))
