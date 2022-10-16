(defpackage :push-constant
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:test.util)
  (:export
   #:push-constant))

(in-package :push-constant)

(defparameter *vert* (make-shader-path "push-constant" :vert))
(defparameter *frag* (make-shader-path "push-constant" :frag))

(defbuffer vertex (:usage :vertex)
  (pos :vec3))

(defbuffer sphere (:usage :push-constant :count 16)
  (pos :vec4)
  (color :vec4))

(defparameter *veretx-data*
  (vector (make-vertex :pos #(0.0 -0.2 0.0))
	  (make-vertex :pos #(0.2 0.2 0.0))
	  (make-vertex :pos #(-0.2 0.2 0.0))))

(defparameter *sphere-data*  ;; build constant data
  (eval `(vector ,@(loop :for i :from 0 :below 16
			 :for rad := (rtg-math:radians (/ (* i 360.0) 16))
			 :collect (make-sphere                ;; triangle position
				   :pos (vector
					 (sin rad)
					 (cos rad)
					 0.0
					 1.0)
				   :color (vector             ;; random color
					   (random 1.0)
					   (random 1.0)
					   (random 1.0)
					   (random 1.0)))))))

(defpipeline-layout push-constant-layout ()
  ()
  ((:flag :vertex                                             ;; push constant info
    :offset 0
    :size (sphere-size))))

(defgpipeline push-constant-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage vertex))

(defstruct push-constant-handle
  pipeline pipeline-layout spheres vertex)

(defun spheres-init (sys)
  (let* ((layout (makepl-push-constant-layout sys))
	 (pipeline (makeg-push-constant-pipeline sys layout)))
    (make-push-constant-handle
     :pipeline pipeline
     :pipeline-layout layout
     :spheres (createc-sphere *sphere-data*)                 ;; create sphere data
     :vertex (createv-vertex sys *veretx-data*))))

(defun spheres-deinit (sys instance)
  (destroy-buffer sys (vbuffer-buffer (push-constant-handle-vertex instance)))
  (destroyc-sphere (push-constant-handle-spheres instance))
  (destroypl-push-constant-layout sys (push-constant-handle-pipeline-layout instance) nil)
  (destroyg-push-constant-pipeline sys (push-constant-handle-pipeline instance)))

(defun draw-spheres (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
    (bind-gpipeline cmd (push-constant-handle-pipeline instance))
    (set-vertex cmd (push-constant-handle-vertex instance))
    (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0))
    (set-scissor cmd :width 800 :height 800)
    (loop :for i :from 0 :below 16
	  :do (progn
		(set-constant cmd (push-constant-handle-pipeline-layout instance) (sphere-size) (slot-ptr-sphere (push-constant-handle-spheres instance) i))   ;; push constant data
		(draw cmd :buffer (push-constant-handle-vertex instance))))))

(defun push-constant ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "push constant")
    (let ((instance (spheres-init sys)))
      (draw-spheres sys instance)
      (with-we-main-loop ())
      (spheres-deinit sys instance))))
