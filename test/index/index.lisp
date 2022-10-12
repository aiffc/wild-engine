(defpackage :index
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:test.util)
  (:export
   #:index))

(in-package #:index)

(defparameter *vert* (make-shader-path "index" :vert))
(defparameter *frag* (make-shader-path "index" :frag))

(defbuffer vertex (:usage :vertex)
  (pos :vec3)
  (color :vec3))

(defparameter *vertex-data*
  (vector (make-vertex :pos #(-0.5 -0.5 0.0)
		       :color #(1.0 0.0 0.0))
	  (make-vertex :pos #(0.5 -0.5 0.0)
		       :color #(0.0 1.0 0.0))
	  (make-vertex :pos #(0.5 0.5 0.0)
		       :color #(0.0 0.0 1.0))
	  (make-vertex :pos #(-0.5 0.5 0.0)
		       :color #(0.0 0.0 0.0))))

(defparameter *index-data* (vector 0 1 2 2 3 0))

(defpipeline-layout index-pipeline-layout ())

(defgpipeline index-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage vertex))

(defstruct index-handle
  pipeline vertex index pipeline-layout)

(defun index-init (sys)
  (let* ((layout (makepl-index-pipeline-layout sys))
	 (pipeline (makeg-index-pipeline sys layout)))
    (make-index-handle
     :pipeline pipeline
     :pipeline-layout layout
     :vertex (createv-vertex sys *vertex-data*)
     :index (create-index-buffer sys *index-data*))))       ;; create index buffer

(defun index-deinit (sys instance)
  (destroyg-index-pipeline sys (index-handle-pipeline instance))
  (destroypl-index-pipeline-layout sys (index-handle-pipeline-layout instance) nil)
  (destroy-buffer sys (ibuffer-buffer (index-handle-index instance)))
  (destroy-buffer sys (vbuffer-buffer (index-handle-vertex instance))))

(defun draw-index (sys instance)
  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0)) ;; record command buffer
    (bind-gpipeline cmd (index-handle-pipeline instance))
    (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0))
    (set-scissor cmd :width 800 :height 800)
    (set-vertex cmd (index-handle-vertex instance))
    (set-index cmd (index-handle-index instance))                   ;; bind index buffer
    (draw cmd :buffer (index-handle-index instance) :index-p t)))   ;; draw with index buffer

(defun index ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "index-buffer")
    (let ((index-instance (index-init sys)))
      (draw-index sys index-instance)
      (with-we-main-loop ())
      (index-deinit sys index-instance))))
