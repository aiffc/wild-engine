(in-package :we.test)


(defparameter *test-path* (namestring (asdf:system-relative-pathname :wild-engine "test/")))
(defparameter *vert* (concatenate 'string *test-path* "vert.spv"))
(defparameter *frag* (concatenate 'string *test-path* "frag.spv"))

(defbuffer test-vertex (:usage :vertex)
  (pos :vec2)
  (color :vec3))

(defpipeline-layout test-pipeline-layout ())

(defgpipeline test-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage test-vertex))

(defun test ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "test")
    (withpl-test-pipeline-layout (layout sys)
      (withg-pipelines (sys layout
			    (pipeline test-pipeline 1)
			    (pipeline2 test-pipeline 1))
	(with-we-main-loop ()
	  (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))
	    
	    (vk:cmd-bind-pipeline cmd :graphics pipeline)
	    (vk:cmd-set-viewport cmd 0 (list (vk:make-viewport :width 200.0
							       :height 200.0
							       :max-depth 1.0)))
	    (vk:cmd-set-scissor cmd 0 (list (vk:make-rect-2d
					     :offset (vk:make-offset-2d :x 0
									:y 0)
					     :extent (vk:make-extent-2d :width 200
									:height 200))))
	    (vk:cmd-draw cmd 3 1 0 0)

	    (vk:cmd-bind-pipeline cmd :graphics pipeline2)
	    (vk:cmd-set-viewport cmd 0 (list (vk:make-viewport :width 400.0 :height 400.0 :max-depth 1.0)))
	    (vk:cmd-set-scissor cmd 0 (list (vk:make-rect-2d
					     :offset (vk:make-offset-2d :x 0
									:y 0)
					     :extent (vk:make-extent-2d :width 400
									:height 400))))
	    (vk:cmd-draw cmd 3 1 0 0)))))))



