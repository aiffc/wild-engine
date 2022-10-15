(defpackage :model
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:we.model #:test.util)
  (:export
   #:model))

(in-package #:model)

(defparameter *vert* (make-shader-path "model" :vert))
(defparameter *frag* (make-shader-path "model" :frag))
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

(defpipeline-layout model-pipeline-layout ()
  ((:type :uniform-buffer
    :count 1
    :flags :vertex
    :samplers nil)
   (:type :combined-image-sampler
    :count 1
    :flags :fragment)))

(defdescriptor model-descriptor-pool (1)
  (:type :uniform-buffer
   :count 1
   :flags :vertex
   :samplers nil)
  (:type :combined-image-sampler
   :count 1
   :flags :fragment))

(defdescriptor-sets model-descriptor-set)

(defgpipeline model-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage mesh-node)
  (:topology :triangle-list))

(deftexture model-texture *model-texture* :rgba)

(defstruct model-handle
  pipeline vertex index pipeline-layout descriptor-layout descriptor-pool descriptor-set texture uniform)

(defun model-init (sys)
  (multiple-value-bind (layout descriptor-layout)
      (makepl-model-pipeline-layout sys)
    (let ((pipeline (makeg-model-pipeline sys layout))
	  (pool (makedp-model-descriptor-pool sys)))
    (multiple-value-bind (vertex indices) (load-mesh *model*)
      (make-model-handle
       :pipeline pipeline
       :pipeline-layout layout
       :descriptor-layout descriptor-layout
       :descriptor-pool pool
       :descriptor-set (allocds-model-descriptor-set sys pool descriptor-layout)
       :vertex (createv-mesh-node sys vertex)
       :texture (maket-model-texture sys)
       :index (create-index-buffer sys indices)
       :uniform (createu-uniform sys 1))))))

(defun model-deinit (sys instance)
  (freeds-model-descriptor-set sys (model-handle-descriptor-pool instance) (model-handle-descriptor-set instance))
  (destroydp-model-descriptor-pool sys (model-handle-descriptor-pool instance))
  (destroyg-model-pipeline sys (model-handle-pipeline instance))
  (destroypl-model-pipeline-layout sys (model-handle-pipeline-layout instance) (model-handle-descriptor-layout instance))
  (destroy-buffer sys (vbuffer-buffer (model-handle-vertex instance)))
  (destroy-buffer sys (ibuffer-buffer (model-handle-index instance)))
  (destroy-image sys (model-handle-texture instance))
  (destroyu-uniform sys (model-handle-uniform instance)))

(defun draw-model (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
    (bind-gpipeline cmd (model-handle-pipeline instance))
    (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0))
    (set-scissor cmd :width 800 :height 800)
    (set-vertex cmd (model-handle-vertex instance))
    (set-index cmd (model-handle-index instance))
    (bind-descriptor-sets cmd (model-handle-pipeline-layout instance) (model-handle-descriptor-set instance))
    (draw cmd :buffer (model-handle-index instance) :index-p t)))

(defun model ()
  (setf *ticks* 0)
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "model loader" :anti-aliasing t)
    (let ((model-instance (model-init sys)))
      (updateds-buffer-model-descriptor-set sys
					    (model-handle-descriptor-set model-instance)
					    (model-handle-uniform model-instance)
					    :binding 0
					    :range (cffi:foreign-type-size '(:struct uniform)))
      (updateds-image-model-descriptor-set sys
					   (model-handle-descriptor-set model-instance)
					   (model-handle-texture model-instance)
					   :binding 1)
      (with-we-main-loop ()
	(:idle () (progn
		    (update-uniform-data)
		    (updateu-uniform sys (model-handle-uniform model-instance) 0 *uniform-data*)
		    (draw-model sys model-instance))))
      (model-deinit sys model-instance))))
