(defpackage :descriptor-sets
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:test.util)
  (:export
   #:index))

(in-package #:descriptor-sets)

(defparameter *vert* (make-shader-path "descriptor-sets" :vert))
(defparameter *frag* (make-shader-path "descriptor-sets" :frag))
(defparameter *texture-path* (make-texture-path "descriptor-sets" "test.png"))

(defbuffer vertex (:usage :vertex)
  (pos :vec3)
  (text-coord :vec2))

(defparameter *vertex-data*
  (vector (make-vertex :pos #(-0.5 -0.5 0.0)
		       :text-coord #(0.0 0.0))
	  (make-vertex :pos #(0.5 -0.5 0.0)
		       :text-coord #(1.0 0.0))
	  (make-vertex :pos #(0.5 0.5 0.0)
		       :text-coord #(1.0 1.0))
	  (make-vertex :pos #(-0.5 0.5 0.0)
		       :text-coord #(0.0 1.0))))

(defparameter *index-data* (vector 0 1 2 2 3 0))

(defbuffer uniform (:usage :uniform)
  (model :mat4))

(defparameter *uniform-angle* 0.0)
(defparameter *uniform-data*
  (make-uniform :model (m4:rotation-from-axis-angle (v3:make 1.0 0.0 0.0) *uniform-angle*)))
(defparameter *ticks* (sdl2:get-ticks))

(defun update-uniform-data ()
  "update uniform data"
  (when (> (sdl2:get-ticks) (+ *ticks* 30))
    (incf *uniform-angle* 0.01)
    (setf (model *uniform-data*)
	  (m4:make (cos *uniform-angle*) (- (sin *uniform-angle*)) 0.0 0.0
		   (sin *uniform-angle*) (cos *uniform-angle*) 0.0 0.0
		   0.0 0.0 1.0 0.0
		   0.0 0.0 0.0 1.0)
	  *ticks* (sdl2:get-ticks))))

(deftexture texture *texture-path* :rgba)

(defpipeline-layout descriptor-pipeline-layout ()
  ((:type :uniform-buffer                       ;; uniform buffer
    :count 1
    :flags :vertex
    :samplers nil)
   (:type :combined-image-sampler               ;; image sampler
    :count 1
    :flags :fragment)))

(defdescriptor descriptor-pool (1)              ;; max sets
  (:type :uniform-buffer
   :count 1
   :flags :vertex
   :samplers nil)
  (:type :combined-image-sampler
   :count 1
   :flags :fragment))

(defdescriptor-sets descriptor-set)            ;; descriptor set defination

(defgpipeline descriptor-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage vertex))

(defstruct descriptor-set-handle
  pipeline
  vertex
  index
  pipeline-layout
  descriptor-layout
  descripotr-pool
  descriptor-set
  texture
  uniform)

(defun descriptor-sets-init (sys)
  (multiple-value-bind (layout descriptor-layout)
      (makepl-descriptor-pipeline-layout sys)                ;; create pipeline layout and descripotr set layout
    (let ((pipeline (makeg-descriptor-pipeline sys layout))
	  (pool (makedp-descriptor-pool sys)))               ;; create descriotor pool
      (make-descriptor-set-handle
       :pipeline pipeline
       :pipeline-layout layout
       :descriptor-layout descriptor-layout
       :descripotr-pool pool
       :descriptor-set (allocds-descriptor-set sys pool descriptor-layout) ;; allocate descriptor sets
       :vertex (createv-vertex sys *vertex-data*)
       :index (create-index-buffer sys *index-data*)
       :texture (maket-texture sys)                          ;; create texture
       :uniform (createu-uniform sys 1)))))                  ;; create uniform buffer

(defun descriptor-sets-deinit (sys instance)
  (freeds-descriptor-set sys (descriptor-set-handle-descripotr-pool instance) (descriptor-set-handle-descriptor-set instance))                                            ;; free descriptor set
  (destroydp-descriptor-pool sys (descriptor-set-handle-descripotr-pool instance)) ;; destroy descriptor pool
  (destroyg-descriptor-pipeline sys (descriptor-set-handle-pipeline instance))
  (destroypl-descriptor-pipeline-layout sys (descriptor-set-handle-pipeline-layout instance) (descriptor-set-handle-descriptor-layout instance))                          ;; destroy pipeline layout and descriptor set layout
  (destroy-buffer sys (ibuffer-buffer (descriptor-set-handle-index instance)))
  (destroy-buffer sys (vbuffer-buffer (descriptor-set-handle-vertex instance)))
  (destroyu-uniform sys (descriptor-set-handle-uniform instance)) ;; destroy uniform buffer
  (destroy-image sys (descriptor-set-handle-texture instance)))  ;; destroy texture 

(defun draw-descriptor (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0)) ;; record command buffer
    (bind-gpipeline cmd (descriptor-set-handle-pipeline instance))
    (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0))
    (set-scissor cmd :width 800 :height 800)
    (set-vertex cmd (descriptor-set-handle-vertex instance))
    (set-index cmd (descriptor-set-handle-index instance))
    (bind-descriptor-sets cmd (descriptor-set-handle-pipeline-layout instance) (descriptor-set-handle-descriptor-set instance))              ;; bind descriptor buffer
    (draw cmd :buffer (descriptor-set-handle-index instance) :index-p t)))

(defun descriptor-set ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "descriptor set")
    (let ((descriptor-set-instance (descriptor-sets-init sys)))
      ;; update uniform buffer binding 0
      (updateds-buffer-descriptor-set sys
				      (descriptor-set-handle-descriptor-set descriptor-set-instance)
				      (descriptor-set-handle-uniform descriptor-set-instance)
				      :binding 0
				      :range (cffi:foreign-type-size '(:struct uniform)))
      ;; update uniform buffer binding 1
      (updateds-image-descriptor-set sys
				     (descriptor-set-handle-descriptor-set descriptor-set-instance)
				     (descriptor-set-handle-texture descriptor-set-instance)
				     :binding 1)
      (with-we-main-loop ()
	(:idle () (progn
		    (update-uniform-data)
		    (updateu-uniform sys (descriptor-set-handle-uniform descriptor-set-instance) 0 *uniform-data*)
		    (draw-descriptor sys descriptor-set-instance))))
      (descriptor-sets-deinit sys descriptor-set-instance))))
