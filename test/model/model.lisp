(defpackage :model
  (:use #:cl #:we.vk #:we.math #:we.ctrl #:we.model #:test.util)
  (:export
   #:model))

(in-package #:model)

(defparameter *vert* (make-shader-path "model" :vert))
(defparameter *frag* (make-shader-path "model" :frag))
(defparameter *model* "/home/aif/bunny.obj")

(defpipeline-layout model-pipeline-layout ())

;; (defgpipeline model-pipeline ()
;;   (:vertex *vert*)
;;   (:fragment *frag*)
;;   (:vertex-stage mesh-node)
;;   (:topology :line-list))

;; (defstruct model-handle
;;   pipeline vertex index pipeline-layout)

;; (defun model-init (sys)
;;   (let* ((layout (makepl-model-pipeline-layout sys))     ;; init pipeline layout
;; 	 (pipeline (makeg-model-pipeline sys layout)))   ;; init pipeline
;;     (multiple-value-bind (vertex indices) (load-mesh *model*)
;;       (make-model-handle
;;        :pipeline pipeline
;;        :pipeline-layout layout
;;        :vertex (createv-mesh-node sys vertex)
;;        :index (create-index-buffer sys indices :uint64)))))

;; (defun model-deinit (sys instance)
;;   (destroyg-model-pipeline sys (model-handle-pipeline instance))
;;   (destroypl-model-pipeline-layout sys (model-handle-pipeline-layout instance) nil)
;;   (destroy-buffer sys (vbuffer-buffer (model-handle-vertex instance)))
;;   (destroy-buffer sys (ibuffer-buffer (model-handle-index instance))))

;; (defun draw-model (sys instance)
;;   (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
;;     (bind-gpipeline cmd (model-handle-pipeline instance))
;;     (set-viewport cmd :width (* 1.0 800.0) :height (* 1.0 800.0))
;;     (set-scissor cmd :width 800 :height 800)
;;     (set-vertex cmd (model-handle-vertex instance))
;;     (set-index cmd (model-handle-index instance))
;;     (draw cmd :buffer (model-handle-index instance) :index-p t)))

;; (defun model ()
;;   (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "model")
;;     (let ((model-instance (model-init sys)))
;;       (draw-model sys model-instance)
;;       (with-we-main-loop ())
;;       (model-deinit sys model-instance))))
