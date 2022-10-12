(defpackage :triangle
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:test.util)
  (:export
   #:triangle))

(in-package #:triangle)

(defparameter *vert* (make-shader-path "triangle" :vert))
(defparameter *frag* (make-shader-path "triangle" :frag))

(defbuffer vertex (:usage :vertex)
  (pos :vec3)
  (color :vec3))

;; vertex buffer data
(defparameter *vertex-data*
  (vector (make-vertex :pos #(0.0 -0.5 0.0)
		       :color #(1.0 0.0 0.0))
	  (make-vertex :pos #(0.5 0.5 0.0)
		       :color #(0.0 1.0 0.0))
	  (make-vertex :pos #(-0.5 0.5 0.0)
		       :color #(0.0 0.0 1.0))))

;; define pipeline layout 
(defpipeline-layout vertex-pipeline-layout ())

;; define pipeline
(defgpipeline vertex-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage vertex))

(defstruct triangle-handle
  pipeline vertex pipeline-layout)

(defun triangle-init (sys)
  (let* ((layout (makepl-vertex-pipeline-layout sys))     ;; init pipeline layout
	 (pipeline (makeg-vertex-pipeline sys layout)))   ;; init pipeline
    (make-triangle-handle
	 :pipeline pipeline
	 :pipeline-layout layout
	 :vertex (createv-vertex sys *vertex-data*))))    ;; init vertex buffer

(defun triangle-deinit (sys instance)
  (destroyg-vertex-pipeline sys (triangle-handle-pipeline instance))
  (destroypl-vertex-pipeline-layout sys (triangle-handle-pipeline-layout instance) nil)
  (destroy-buffer sys (vbuffer-buffer (triangle-handle-vertex instance))))

(defun draw-triangle (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0)) ;; record command buffer
    (bind-gpipeline cmd (triangle-handle-pipeline instance))      ;; bind pipeline
    (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0)) ;; set viewport
    (set-scissor cmd :width 800 :height 800)                      ;; set scissor
    (set-vertex cmd (triangle-handle-vertex instance))            ;; bind vertex buffer
    (draw cmd :buffer (triangle-handle-vertex instance))))        ;; draw with vertex buffer

(defun triangle ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "triangle")
    (let ((triangle-instance (triangle-init sys)))
      (draw-triangle sys triangle-instance)
      (with-we-main-loop ())
      (triangle-deinit sys triangle-instance))))
