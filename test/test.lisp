(in-package :we.test)


(defparameter *test-path* (namestring (asdf:system-relative-pathname :wild-engine "test/")))
(defparameter *vert* (concatenate 'string *test-path* "vert.spv"))
(defparameter *frag* (concatenate 'string *test-path* "frag.spv"))

(defbuffer test-vertex (:usage :vertex)
  (pos :vec3)
  (color :vec3)
  (normal :vec3))

(defparameter *vertex-data*
  (vector (make-test-vertex :pos #(0.0 -0.5 0.0)
			    :color #(1.0 0.0 0.0)
			    :normal #(0.0 0.0 0.0))
	  (make-test-vertex :pos #(0.5 0.5 0.0)
			    :color #(0.0 1.0 0.0)
			    :normal #(0.0 0.0 0.0))
	  (make-test-vertex :pos #(-0.5 0.5 0.0)
			    :color #(0.0 0.0 1.0)
			    :normal #(0.0 0.0 0.0))))

(defpipeline-layout test-pipeline-layout ())

(defgpipeline test-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage test-vertex))

(defun test ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "test")
    (withpl-test-pipeline-layout (layout sys)
      (withg-pipelines (sys layout
			    (pipeline test-pipeline :vertex))
	(withv-test-vertex (sys *vertex-data* buffer memory size)
	  (with-we-main-loop ()
	    (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
	      (vk:cmd-bind-pipeline cmd :graphics pipeline)
	      (vk:cmd-bind-vertex-buffers cmd 0 (list buffer) (list 0))
	      (vk:cmd-set-viewport cmd 0 (list (vk:make-viewport :width 800.0
								 :height 800.0
								 :max-depth 1.0)))
	      (vk:cmd-set-scissor cmd 0 (list (vk:make-rect-2d
					       :offset (vk:make-offset-2d :x 0
									  :y 0)
					       :extent (vk:make-extent-2d :width 800
									  :height 800))))
	      (vk:cmd-draw cmd size 1 0 0))))
	))))



