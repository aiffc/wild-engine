(in-package :we.test2)

(we.dbg:dbg-trace :vk)

(defparameter *test-path* (namestring (asdf:system-relative-pathname :wild-engine "test/test2/")))
(defparameter *vert* (concatenate 'string *test-path* "vert.spv"))
(defparameter *frag* (concatenate 'string *test-path* "frag.spv"))

(defbuffer test-vertex (:usage :vertex)                ;; vertex buffer defination
  (pos :vec3)
  (color :vec3))

(defparameter *vertex-data*
  (vector (make-test-vertex :pos #(-1.0 -1.0  1.0) ;; make test-vertex data
			    :color #(1.0 0.0 0.0))
	  (make-test-vertex :pos #(1.0 -1.0  1.0)
			    :color #(0.0 1.0 0.0))
	  (make-test-vertex :pos #(1.0  1.0  1.0)
			    :color #(0.0 0.0 1.0))
	  (make-test-vertex :pos #(-1.0  1.0  1.0)
			    :color #(0.0 0.0 0.0))
	  (make-test-vertex :pos #(-1.0 -1.0 -1.0)
			    :color #(1.0 0.0 0.0))
	  (make-test-vertex :pos #(1.0 -1.0 -1.0)
			    :color #(0.0 1.0 0.0))
	  (make-test-vertex :pos #(1.0  1.0 -1.0)
			    :color #(0.0 0.0 1.0))
	  (make-test-vertex :pos #(-1.0  1.0 -1.0)
			    :color #(0.0 0.0 0.0))))

(defparameter *index-data* (vector 0 1 2  2 3 0  1 5 6  6 2 1  7 6 5  5 4 7  4 0 3  3 7 4  4 5 1  1 0 4  3 2 6  6 7 3))     ;; index buffer data

(defbuffer ubovs (:usage :uniform)
  (proj :mat4)
  (view :mat4))

(cffi:defcstruct (ubodynamics :class c-ubodynamics)
  (module (:pointer (:struct we.math:mat4))))

(defparameter *ptr* nil)
(defparameter *dynamic-ubo-count* 125)
(defparameter *dynamic-alignment* 0)

(defparameter *uniform-data*
  (make-ubovs :view (m4:look-at (v3:make 0.0 0.0 1.0)
				(v3:make 2.0 2.0 2.0)
				(v3:make 0.0 0.0 0.0))
	      :proj (rtg-math.projection:perspective 600.0 600.0 0.1 10.0 -45.0)))

(defparameter *uniform-angle* (loop :for i :from 0 :below *dynamic-ubo-count*
				    :collect (random 90.0)))

(defun prepare-dynamic-uniform-buffer (sys
				       &aux
					 (min-aligment (vk:min-uniform-buffer-offset-alignment
							(vk:limits (we.vk::get-gpu-properties sys))))
					 (alignment (cffi:foreign-type-size '(:struct ubodynamics))))
  (when (> min-aligment 0)
    (setf alignment (logand (+ alignment (- min-aligment 1))
			    (lognot (- min-aligment 1)))))
  (setf *dynamic-alignment* alignment)
  (let ((ptr (cffi:foreign-alloc '(:struct ubodynamics))))
    (setf (cffi:foreign-slot-value ptr '(:struct ubodynamics) 'module)
	  (cffi::%foreign-alloc (* alignment *dynamic-ubo-count*)))
    (multiple-value-bind (buffer memory)
	(we.vk::create-buffer sys (* alignment *dynamic-ubo-count*) :uniform-buffer :host-visible)
      (values ptr buffer memory))))

(defun update-dynamic-uniform-buffer (sys memory ptr)
  (loop :for i :from 0 :below *dynamic-ubo-count*
	:do (progn
	      (setf (cffi:mem-ref (cffi:inc-pointer (cffi:foreign-slot-value ptr '(:struct ubodynamics) 'module)
						    (* i *dynamic-alignment*))
				  '(:struct we.math:mat4))
		    (m4:* (m4:rotation-from-axis-angle (v3:make 0.0 0.0 1.0) (incf (nth i *uniform-angle*)))
			  (m4:make 0.1 0.0 0.0 (float (/ i 10))
				   0.0 0.1 0.0 0.0
				   0.0 0.0 0.1 0.0
				   0.0 0.0 0.0 1.0)))))
  (we.vk::map-memory sys memory (cffi:foreign-slot-value ptr '(:struct ubodynamics) 'module)
		     (* *dynamic-alignment* *dynamic-ubo-count*)))

(defun update-dynamic-uniform-bufferd (sys sets buffer 
				       &key
					 (index 0)
					 (offset 0)
					 (array-element 0)
				       &aux (device (we.vk::get-device sys)))
  ;;(format t "~a~%" (we.vk::get-uniform-buffer-by-index buffer index))
  (let* ((buffer-info (vk:make-descriptor-buffer-info
		       :buffer buffer
		       :offset offset
		       :range *dynamic-alignment*))
	 (write (vk:make-write-descriptor-set
		 :dst-set (nth index sets) 
		 :dst-binding 1
		 :dst-array-element array-element
		 :descriptor-type :uniform-buffer-dynamic
		 :buffer-info (list buffer-info))))
    (we.dbg:msg :app "write buffer ~a to sets ~a ~%" buffer (nth index sets))
    (vk:update-descriptor-sets device (list write) nil)))

(defgpipeline test-pipeline ()                       ;; graphics pipeline defination
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage test-vertex))

(defpipeline-layout test-pipeline-layout ()
  (:type :uniform-buffer
   :count 1
   :flags :vertex
   :samplers nil)
  (:type :uniform-buffer-dynamic
   :count 1
   :flags :vertex))

(defdescriptor descriptor (1)
  (:type :uniform-buffer
   :count 1)
  (:type :uniform-buffer-dynamic
   :count 1))

(defdescriptor-sets set0 1)

(defun test2 ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "test")  ;; we initialize
    ;; ready buffers
    (withu-ubovs (uniforms sys 1)
      (withv-test-vertex (vertex-buffer vertex-size sys *vertex-data*)  ;; vertex data instance
	(declare (ignore vertex-size))
	(with-index-buffer (index-buffer sys *index-data*)
	  ;; ready pipeline layuout
	  (withpl-test-pipeline-layout (layout descriptor-layout sys) ;; pipeline layout initialize
	    (multiple-value-bind (duptr dubuffer dumemory) (prepare-dynamic-uniform-buffer sys)
	      ;; ready descriptor and descriptor sets)
	      (withdp-descriptor (descriptor-pool sys)
		(withds-set0 (sets sys descriptor-pool descriptor-layout)
		  (updateds-buffer-set0 sys sets uniforms
					:range (cffi:foreign-type-size '(:struct ubovs))
					:type :uniform-buffer)
		  (update-dynamic-uniform-bufferd sys sets dubuffer)
		  ;; ready graphics pipeline
		  (withg-pipelines (sys layout                           ;; make pipeline instance
					(pipeline test-pipeline :vertex))
		    (updateu-ubovs sys uniforms 0 *uniform-data*)
		    (with-we-main-loop ()
		      (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0)) ;; record command buffer
			(update-dynamic-uniform-buffer sys dumemory duptr)
			(bind-gpipeline cmd pipeline)
			(set-vertex cmd vertex-buffer)
			(set-index cmd index-buffer)
			(set-viewport cmd :width 800.0 :height 800.0)
			(set-scissor cmd :width 800 :height 800)
			(loop :for i :from 0 :below *dynamic-ubo-count*
			      :do (progn
				    (bind-descriptor-sets cmd layout sets (list (* i *dynamic-alignment*)))
				    (draw cmd :buffer index-buffer :index-p t)))))
		    (cffi:foreign-free (cffi:foreign-slot-value duptr '(:struct ubodynamics) 'module))
		    (cffi:foreign-free duptr)))))))))))
