(defpackage :multi-pipeline
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:we.model #:test.util))

(in-package #:multi-pipeline)

(defparameter *vert* (make-shader-path "multi-pipeline" :vert))
(defparameter *frag* (make-shader-path "multi-pipeline" :frag))
(defparameter *model* (make-model-path "spot"))
(defparameter *model-texture* (make-model-texture-path "spot"))

(defbuffer uniform (:usage :uniform)
  (mt :mat4)
  (pt :mat4)
  (vt :mat4))

(defparameter *uniform-angle* 0.0)
(defparameter *uniform-data*
  (make-uniform :mt (m4:rotation-from-axis-angle (v3:make 1.0 0.0 0.0) *uniform-angle*)
		:vt (m4:look-at (v3:make 0.0 0.0 1.0)
				(v3:make 2.0 2.0 2.0)
				(v3:make 0.0 0.0 0.0))
		:pt (rtg-math.projection:perspective 800.0 800.0 0.1 10.0 45.0)))
(defparameter *ticks* (sdl2:get-ticks))

(defun update-uniform-data ()
  (when (> (sdl2:get-ticks) (+ *ticks* 30))
    (incf *uniform-angle* 0.01)
    (setf (mt *uniform-data*)
	  (m4:make (cos *uniform-angle*)  0.0 (- (sin *uniform-angle*)) 0.0
		   0.0 1.0 0.0 0.0
		   (sin *uniform-angle*) 0.0 (cos *uniform-angle*) 0.0
		   0.0 0.0 0.0 1.0)
	  *ticks* (sdl2:get-ticks))))

(defpipeline-layout multi-pipeline-layout ()
  ((:type :uniform-buffer
    :count 1
    :flags :vertex
    :samplers nil)
   (:type :combined-image-sampler
    :count 1
    :flags :fragment)))

(defdescriptor multi-pipeline-descriptor-pool (1)
  (:type :uniform-buffer
   :count 1
   :flags :vertex
   :samplers nil)
  (:type :combined-image-sampler
   :count 1
   :flags :fragment))

(defdescriptor-sets multi-pipeline-descriptor-set)

(defgpipeline pipeline1 ()   ;; pipeline1
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage mesh-node)
  (:topology :triangle-list))

(defgpipeline pipeline2 ()   ;; pipeline2
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage mesh-node)
  (:topology :line-list))

(deftexture multi-pipeline-texture *model-texture* :rgba)

(defstruct multi-pipeline-handle
  pipeline1 pipeline2 pipeline-layout descriptor-layout descriptor-pool descriptor-set
  vertex index texture uniform)

(defun multi-pipeline-init (sys)
  (multiple-value-bind (layout descriptor-layout)
      (makepl-multi-pipeline-layout sys)
    (let ((pipeline1 (makeg-pipeline1 sys layout))            ;; create pipeline1
	  (pipeline2 (makeg-pipeline2 sys layout))            ;; create pipeline2
	  (pool (makedp-multi-pipeline-descriptor-pool sys)))
    (multiple-value-bind (vertex indices) (load-mesh *model*)
      (make-multi-pipeline-handle
       :pipeline1 pipeline1
       :pipeline2 pipeline2
       :pipeline-layout layout
       :descriptor-layout descriptor-layout
       :descriptor-pool pool
       :descriptor-set (allocds-multi-pipeline-descriptor-set sys pool descriptor-layout)
       :vertex (createv-mesh-node sys vertex)
       :texture (maket-multi-pipeline-texture sys)
       :index (create-index-buffer sys indices)
       :uniform (createu-uniform sys 1))))))

(defun multi-pipeline-deinit (sys instance)
  (freeds-multi-pipeline-descriptor-set sys (multi-pipeline-handle-descriptor-pool instance) (multi-pipeline-handle-descriptor-set instance))
  (destroydp-multi-pipeline-descriptor-pool sys (multi-pipeline-handle-descriptor-pool instance))
  (destroyg-pipeline1 sys (multi-pipeline-handle-pipeline1 instance))
  (destroyg-pipeline2 sys (multi-pipeline-handle-pipeline2 instance))
  (destroypl-multi-pipeline-layout sys (multi-pipeline-handle-pipeline-layout instance) (multi-pipeline-handle-descriptor-layout instance))
  (destroy-buffer sys (vbuffer-buffer (multi-pipeline-handle-vertex instance)))
  (destroy-buffer sys (ibuffer-buffer (multi-pipeline-handle-index instance)))
  (destroy-image sys (multi-pipeline-handle-texture instance))
  (destroyu-uniform sys (multi-pipeline-handle-uniform instance)))

(defun draw-pipeline (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
    (set-vertex cmd (multi-pipeline-handle-vertex instance))
    (set-index cmd (multi-pipeline-handle-index instance))
    (bind-descriptor-sets cmd (multi-pipeline-handle-pipeline-layout instance) (multi-pipeline-handle-descriptor-set instance))
    (set-scissor cmd :width 800 :height 800)
    ;; draw pipeline1
    (set-viewport cmd :width (/ 800.0 2.0) :height 800.0)      ;; left
    (bind-gpipeline cmd (multi-pipeline-handle-pipeline1 instance))
    (draw cmd :buffer (multi-pipeline-handle-index instance) :index-p t)
    ;; draw pipeline2
    (set-viewport cmd :width (/ 800.0 2.0) :height 800.0 :x (/ 800.0 2.0)) ;; right
    (set-line-width cmd 3.0)
    (bind-gpipeline cmd (multi-pipeline-handle-pipeline2 instance))
    (draw cmd :buffer (multi-pipeline-handle-index instance) :index-p t)))

(defun multi-pipeline ()
  (setf *ticks* 0)
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "model loader")
    (let ((instance (multi-pipeline-init sys)))
      (updateds-buffer-multi-pipeline-descriptor-set sys
						     (multi-pipeline-handle-descriptor-set instance)
						     (multi-pipeline-handle-uniform instance)
						     :binding 0
						     :range (cffi:foreign-type-size '(:struct uniform)))
      (updateds-image-multi-pipeline-descriptor-set sys
						    (multi-pipeline-handle-descriptor-set instance)
						    (multi-pipeline-handle-texture instance)
						    :binding 1)
      (with-we-main-loop ()
	(:idle () (progn
		    (update-uniform-data)
		    (updateu-uniform sys (multi-pipeline-handle-uniform instance) 0 *uniform-data*)
		    (draw-pipeline sys instance))))
      (multi-pipeline-deinit sys instance))))

