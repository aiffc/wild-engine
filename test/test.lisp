(in-package :we.test)


(defparameter *test-path* (namestring (asdf:system-relative-pathname :wild-engine "test/")))
(defparameter *vert* (concatenate 'string *test-path* "vert.spv"))
(defparameter *frag* (concatenate 'string *test-path* "frag.spv"))
(defparameter *texture* (concatenate 'string *test-path* "test.png"))

(defbuffer test-vertex (:usage :vertex)                ;; vertex buffer defination
  (pos :vec3)
  (color :vec3)
  (normal :vec3))

(defparameter *vertex-data*
  (vector (make-test-vertex :pos #(-0.5 -0.5 0.0) ;; make test-vertex data
			    :color #(1.0 0.0 0.0)
			    :normal #(1.0 0.0 0.0))
	  (make-test-vertex :pos #(0.5 -0.5 0.0)
			    :color #(0.0 1.0 0.0)
			    :normal #(0.0 0.0 0.0))
	  (make-test-vertex :pos #(0.5 0.5 0.0)
			    :color #(0.0 0.0 1.0)
			    :normal #(0.0 1.0 0.0))
	  (make-test-vertex :pos #(-0.5 0.5 0.0)
			    :color #(1.0 1.0 1.0)
			    :normal #(1.0 1.0 0.0))))

(defbuffer test-uniform (:usage :uniform)
  (model :mat4)
  (view :mat4)
  (proj :mat4))

(defparameter *uniform-data*
  (make-test-uniform :model (m4:rotation-from-axis-angle (v3:make 0.0 0.0 0.0) 45.0)
		     :view (m4:look-at (v3:make 0.0 0.0 1.0)
				      (v3:make 2.0 2.0 2.0)
				      (v3:make 0.0 0.0 0.0))
		     :proj (rtg-math.projection:perspective 600.0 600.0 0.1 10.0 -45.0)))

(defparameter *uniform-angle* 45.0)

(defparameter *index-data* (vector 0 1 2 2 3 0))     ;; index buffer data

(defpipeline-layout test-pipeline-layout ()
  (:type :uniform-buffer
   :count 1
   :flags :vertex
   :samplers nil)
  (:type :combined-image-sampler
   :count 1
   :flags :fragment))

(defgpipeline test-pipeline ()                       ;; graphics pipeline defination
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage test-vertex))

(defun update-uniform-buffer ()
  (setf (model *uniform-data*)
	(m4:rotation-from-axis-angle (v3:make 0.0 0.0 1.0) (incf *uniform-angle* 0.01))))

(defdescriptor descriptor (1)
  (:type :uniform-buffer
   :count 1)
  (:type :combined-image-sampler
   :count 1))

(defdescriptor-sets set0 1)

(deftexture test-texture *texture* :rgba)

(defun test ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "test")  ;; we initialize
    ;; ready buffers
    (withu-test-uniform (uniforms sys 1)
      (witht-test-texture (texture sys)
	(withv-test-vertex (vertex-buffer vertex-size sys *vertex-data*)  ;; vertex data instance
	  (declare (ignore vertex-size))
	  (with-index-buffer (index-buffer index-size sys *index-data*)
	    ;; ready pipeline layuout
	    (withpl-test-pipeline-layout (layout descriptor-layout sys) ;; pipeline layout initialize
	      ;; ready descriptor and descriptor sets
	      (withdp-descriptor (descriptor-pool sys)
		(withds-set0 (sets sys descriptor-pool descriptor-layout)
		  (updateds-buffer-set0 sys sets uniforms
					:range (cffi:foreign-type-size '(:struct test-uniform))
					:type :uniform-buffer)
		  (updateds-image-set0 sys sets texture :binding 1)
		  ;; ready graphics pipeline
		  (withg-pipelines (sys layout                           ;; make pipeline instance
					(pipeline test-pipeline :vertex))
		    (with-we-main-loop ()
		      (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0)) ;; record command buffer
			(update-uniform-buffer)
			(updateu-test-uniform sys uniforms 0 *uniform-data*)
			(bind-gpipeline cmd pipeline)
			(set-vertex cmd vertex-buffer)
			(set-index cmd index-buffer)
			(set-viewport cmd :width 800.0 :height 800.0)
			(set-scissor cmd :width 800 :height 800)
			(vk:cmd-bind-descriptor-sets cmd :graphics layout 0 sets nil)
			(draw cmd :icount index-size :index-p t)))))))))))))



