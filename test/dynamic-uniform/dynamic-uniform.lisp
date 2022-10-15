(defpackage :dynamic-uniform
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:test.util)
  (:export
   #:dynamic-uniform))

(in-package :dynamic-uniform)

(defparameter *vert* (make-shader-path "dynamic-uniform" :vert))
(defparameter *frag* (make-shader-path "dynamic-uniform" :frag))

(defbuffer vertex (:usage :vertex)
  (pos :vec3)
  (color :vec3))

(defbuffer uniform (:usage :uniform)
  (pt :mat4)   ;; project transfer
  (vt :mat4))  ;; view transfer

(defbuffer duniform (:usage :uniform :dynamic-p t :dynamic-count 25) 
  (mt :mat4))  ;; model transfer

(defparameter *vertex-data*
  (vector (make-vertex
	   :pos #(-1.0 -1.0  1.0)
	   :color #(1.0 0.0 0.0))
	  (make-vertex
	   :pos #(1.0 -1.0  1.0)
	   :color #(0.0 1.0 0.0))
	  (make-vertex
	   :pos #(1.0  1.0  1.0)
	   :color #(0.0 0.0 1.0))
	  (make-vertex
	   :pos #(-1.0  1.0  1.0)
	   :color #(1.0 1.0 0.0))
	  (make-vertex
	   :pos #(-1.0 -1.0 -1.0)
	   :color #(1.0 0.0 1.0))
	  (make-vertex
	   :pos #(1.0 -1.0 -1.0)
	   :color #(0.0 1.0 1.0))
	  (make-vertex
	   :pos #(1.0  1.0 -1.0)
	   :color #(1.0 1.0 1.0))
	  (make-vertex
	   :pos #(-1.0  1.0 -1.0)
	   :color #(0.0 0.0 0.0))))
(defparameter *indices-data*
  (vector 0 1 2 2 3 0 1 5 6 6 2 1 7 6 5 5 4 7 4 0 3 3 7 4 4 5 1 1 0 4 3 2 6 6 7 3))
(defparameter *uniform-angles*
  (loop :for i :from 0 :below 25
	:collect (list (v3:make (random 1.0) (random 1.0) (random 1.0)) ;; random direction
		       (* (random 180) 1.0))))
(defparameter *uniform-dataa*
  (make-uniform :pt (rtg-math.projection:perspective 800.0 800.0 0.1 10.0 45.0)
		:vt (m4:look-at (v3:make 1.0 0.0 0.0)
				(v3:make 5.0 5.0 5.0)
				(v3:make 0.0 0.0 0.0))))
(defparameter *ticks* (sdl2:get-ticks))

(defun update-dynamic-uniform (sys instance) ;; update dynamic uniform buffer
  (when (> (sdl2:get-ticks) (+ *ticks* 30))
    (dotimes (x 5)
      (dotimes (y 5)
	(let* ((update-index (+ (* x 5) y))
	       (angle (nth update-index *uniform-angles*)))
	  (updatedu-duniform sys (dynamic-uniform-handle-dynamic-uniform instance) update-index  ;; update uniform data by index
			     (m4:* (m4:make 0.1 0.0 0.0 (+ -0.75 (* 0.5 x))
					    0.0 0.1 0.0 (+ -0.75 (* 0.5 y)) 
					    0.0 0.0 0.1 0.0
					    0.0 0.0 0.0 1.0)
				   (apply #'m4:rotation-from-axis-angle angle)))
	  (incf (second (nth update-index *uniform-angles*)) 0.1)))
      (mapdu-duniform sys (dynamic-uniform-handle-dynamic-uniform instance)))                    ;; map all data to gpu after update data
    (setf *ticks* (sdl2:get-ticks))))

(defpipeline-layout dynamic-uniform-layout ()
  ((:type :uniform-buffer
    :count 1
    :flags :vertex
    :samplers nil) 
   (:type :uniform-buffer-dynamic                         ;; dynamic uniform buffer bindg to 1
    :count 1
    :flags :vertex
    :samplers nil)))

(defdescriptor dynamic-uniform-descriptor (1)
  (:type :uniform-buffer
   :count 1
   :flags :vertex
   :samplers nil)
  (:type :uniform-buffer-dynamic                          ;; dynamic uniform buffer bing to 1
   :count 1
   :flags :vertex
   :samplers nil))

(defdescriptor-sets dynamic-uniform-sets)

(defgpipeline dyanmic-uniform-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage vertex))

(defstruct dynamic-uniform-handle
  pipeline pipeline-layout descriptor-layout descriptor-pool descriptor-sets
  index vertex uniform dynamic-uniform)

(defun init-dynamic-uniform (sys)
  (multiple-value-bind (layout descriptor-layout)
      (makepl-dynamic-uniform-layout sys)
    (let ((pipeline (makeg-dyanmic-uniform-pipeline sys layout))
	  (pool (makedp-dynamic-uniform-descriptor sys)))
      (make-dynamic-uniform-handle
       :pipeline pipeline
       :pipeline-layout layout
       :descriptor-layout descriptor-layout
       :descriptor-pool pool
       :descriptor-sets (allocds-dynamic-uniform-sets sys pool descriptor-layout)
       :vertex (createv-vertex sys *vertex-data*)
       :index (create-index-buffer sys *indices-data*)
       :uniform (createu-uniform sys 1)
       :dynamic-uniform (createdu-duniform sys)))))       ;; create dynamic uniform buffer

(defun deinit-dynamic-uniform (sys instance)
  (freeds-dynamic-uniform-sets sys (dynamic-uniform-handle-descriptor-pool instance)
			       (dynamic-uniform-handle-descriptor-sets instance))
  (destroydp-dynamic-uniform-descriptor sys (dynamic-uniform-handle-descriptor-pool instance))
  (destroyg-dyanmic-uniform-pipeline sys (dynamic-uniform-handle-pipeline instance))
  (destroypl-dynamic-uniform-layout sys (dynamic-uniform-handle-pipeline-layout instance)
				    (dynamic-uniform-handle-descriptor-layout instance))
  (destroy-buffer sys (vbuffer-buffer (dynamic-uniform-handle-vertex instance)))
  (destroy-buffer sys (ibuffer-buffer (dynamic-uniform-handle-index instance)))
  (destroyu-uniform sys (dynamic-uniform-handle-uniform instance))
  (destroydu-duniform sys (dynamic-uniform-handle-dynamic-uniform instance)))  ;; destroy dynamic uniform buffer

(defun draw-dynamic-uniform (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
    (bind-gpipeline cmd (dynamic-uniform-handle-pipeline instance))
    (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0))
    (set-scissor cmd :width 800 :height 800)
    (set-vertex cmd (dynamic-uniform-handle-vertex instance))
    (set-index cmd (dynamic-uniform-handle-index instance))
    (dotimes (i 25)
      (bind-descriptor-sets cmd (dynamic-uniform-handle-pipeline-layout instance) (dynamic-uniform-handle-descriptor-sets instance) (list (* i (alitnmentdu-duniform sys))))  ;; bind descriptor sets offset by alitnment function export by dynamic uniform buffer defination
      (draw cmd :buffer (dynamic-uniform-handle-index instance) :index-p t))))

(defun dynamic-uniform ()
  (setf *ticks* 0)
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "dynamic uniform buffer")
    (let ((dynamic-uniform-instance (init-dynamic-uniform sys)))
      (updateds-buffer-dynamic-uniform-sets sys
					    (dynamic-uniform-handle-descriptor-sets dynamic-uniform-instance)
					    (dynamic-uniform-handle-uniform dynamic-uniform-instance)
					    :binding 0
					    :range (cffi:foreign-type-size '(:struct uniform)))
      (updateds-dynamic-buffer-dynamic-uniform-sets sys
						    (dynamic-uniform-handle-descriptor-sets dynamic-uniform-instance)
						    (dynamic-uniform-handle-dynamic-uniform dynamic-uniform-instance)
						    :binding 1
						    :range (alitnmentdu-duniform sys)) ;; write to set bing 1
      (with-we-main-loop ()
	(:idle () (progn
		    (update-dynamic-uniform sys dynamic-uniform-instance)
		    (draw-dynamic-uniform sys dynamic-uniform-instance))))
      (deinit-dynamic-uniform sys dynamic-uniform-instance))))
