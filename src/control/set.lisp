(in-package :we.ctrl)

(defun bind-gpipeline (cmd pipeline)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:cmd-bind-pipeline cmd :graphics pipeline))

(defun bind-descriptor-sets (cmd layout set)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:cmd-bind-descriptor-sets cmd :graphics layout 0 set nil))

(defun set-viewport (cmd &key
			   (x 0.0)
			   (y 0.0)
			   (width 600.0)
			   (height 600.0)
			   (min-depth 0.0)
			   (max-depth 1.0))
  "just support signle viewport now"
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type float x y width height min-depth max-depth))
  (vk:cmd-set-viewport cmd 0 (list (vk:make-viewport
				    :x x
				    :y y
				    :width width
				    :height height
				    :min-depth min-depth
				    :max-depth max-depth))))

(defun set-scissor (cmd &key
			  (x 0)
			  (y 0)
			  (width 600)
			  (height 600))
  "just support signle scissor now !!!"
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type integer x y width height))
  (vk:cmd-set-scissor cmd 0 (list (vk:make-rect-2d
				   :offset (vk:make-offset-2d
					    :x x :y y)
				   :extent (vk:make-extent-2d
					    :width width :height height)))))

(defun set-vertex (cmd buffer &key
				(offset 0))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type integer offset))
  (vk:cmd-bind-vertex-buffers cmd 0 (list buffer) (list offset)))

(defun set-index (cmd buffer &key (offset 0))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type integer offset))
  (vk:cmd-bind-index-buffer cmd buffer offset :uint32))

(defun draw (cmd &key
		   (vcount 0)
		   (icount 0)
		   (index-p nil))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type integer vcount icount))
  (if index-p
      (vk:cmd-draw-indexed cmd icount 1 0 0 0)
      (vk:cmd-draw cmd vcount 1 0 0)))

