(defpackage :specialization-constants
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:we.model #:test.util)
  (:export
   #:specialization-constants))

(in-package #:specialization-constants)

(defparameter *vert* (make-shader-path "specialization-constants" :vert))
(defparameter *frag* (make-shader-path "specialization-constants" :frag))
(defparameter *model* (make-model-path "spot"))
(defparameter *model-texture* (make-model-texture-path "spot"))

(defbuffer constants (:usage :specialization-data)   ;; constants data defination
  (light-model :uint32)
  (toon :float))

(defbuffer uniform (:usage :uniform)
  (pt :mat4)
  (mt :mat4)
  (vt :mat4)
  (light-pos :vec4))

(defparameter *uniform-angle* 0.0)
(defparameter *uniform-data*
  (make-uniform :mt (m4:make 0.1 0.0 0.0 0.0
			     0.0 0.1 0.0 0.0
			     0.0 0.0 0.1 0.0
			     0.0 0.0 0.0 1.0)
		:vt (m4:look-at (v3:make 0.0 0.0 1.0)
				(v3:make 2.0 2.0 2.0)
				(v3:make 0.0 0.0 0.0))
		:light-pos (v4:make 1.0 -2.0 1.0 0.0)
		:pt (rtg-math.projection:perspective 800.0 800.0 0.1 10.0 60.0)))
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

(defpipeline-layout pipeline-layout ()
  ((:type :uniform-buffer
    :count 1
    :flags :vertex
    :samplers nil)
   (:type :combined-image-sampler
    :count 1
    :flags :fragment)))

(defdescriptor descriptor-pool (1)
  (:type :uniform-buffer
   :count 1
   :flags :vertex
   :samplers nil)
  (:type :combined-image-sampler
   :count 1
   :flags :fragment))

(defdescriptor-sets descriptor-set)

(defgpipeline pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage mesh-node))

(deftexture texture *model-texture* :rgba)

(defstruct handle
  pipeline pipeline-layout descriptor-layout descriptor-pool descriptor-set
  vertex index texture uniform)

(defun specialization-constants-init (sys light-model toon)
  (multiple-value-bind (layout descriptor-layout)
      (makepl-pipeline-layout sys)
    (let* ((constants-ptr (creates-constants (make-constants :light-model light-model :toon toon)))   ;; alloc constant data for pipeline
	   (pipeline (makeg-pipeline sys layout :frag-ety (constants-entry) :frag-ptr constants-ptr)) ;; push constant to pipeline by frag-ety and frat-ptr
	   (pool (makedp-descriptor-pool sys)))
      (destroys-constants constants-ptr)     ;; destroy constant data
      (multiple-value-bind (vertex indices) (load-mesh *model*)
	(make-handle
	 :pipeline pipeline
	 :pipeline-layout layout
	 :descriptor-layout descriptor-layout
	 :descriptor-pool pool
	 :descriptor-set (allocds-descriptor-set sys pool descriptor-layout)
	 :vertex (createv-mesh-node sys vertex)
	 :texture (maket-texture sys)
	 :index (create-index-buffer sys indices)
	 :uniform (createu-uniform sys 1))))))

(defun specialization-constants-deinit (sys instance)
  (freeds-descriptor-set sys (handle-descriptor-pool instance) (handle-descriptor-set instance))
  (destroydp-descriptor-pool sys (handle-descriptor-pool instance))
  (destroyg-pipeline sys (handle-pipeline instance))
  (destroypl-pipeline-layout sys (handle-pipeline-layout instance) (handle-descriptor-layout instance))
  (destroy-buffer sys (vbuffer-buffer (handle-vertex instance)))
  (destroy-buffer sys (ibuffer-buffer (handle-index instance)))
  (destroy-image sys (handle-texture instance))
  (destroyu-uniform sys (handle-uniform instance)))

(defun draw-specialization-constants (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
    (bind-gpipeline cmd (handle-pipeline instance))
    (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0))
    (set-scissor cmd :width 800 :height 800)
    (set-vertex cmd (handle-vertex instance))
    (set-index cmd (handle-index instance))
    (bind-descriptor-sets cmd (handle-pipeline-layout instance) (handle-descriptor-set instance))
    (draw cmd :buffer (handle-index instance) :index-p t)))

(defun specialization-constants (&key (light-model 2) (toon 0.2))
  (setf *ticks* 0)
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "model loader" :anti-aliasing t)
    (let ((instance (specialization-constants-init sys light-model toon)))
      (updateds-buffer-descriptor-set sys
				      (handle-descriptor-set instance)
				      (handle-uniform instance)
				      :binding 0
				      :range (cffi:foreign-type-size '(:struct uniform)))
      (updateds-image-descriptor-set sys
				     (handle-descriptor-set instance)
				     (handle-texture instance)
				     :binding 1)
      (with-we-main-loop ()
	(:idle () (progn
		    (update-uniform-data)
		    (updateu-uniform sys (handle-uniform instance) 0 *uniform-data*)
		    (draw-specialization-constants sys instance))))
      (specialization-constants-deinit sys instance))))

