(in-package :%wild-engine.core.vk)

(defun load-shader-module (path)
  (with-open-file (in path
		      :direction :input
		      :element-type '(unsigned-byte 32))
    (let ((shader-code (make-array 1024 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 32))))
      (loop :for b := (read-byte in nil nil)
	    :while b
	    :do (vector-push-extend b shader-code)
	    :finally (return (adjust-array shader-code (length shader-code)))))))

(defun create-shader-module (sys path 
			     &aux
			       (device (get-device sys)))
  (let* ((create-info (vk:make-shader-module-create-info
		       :code (load-shader-module path)))
	 (module (check-result #'vk:create-shader-module device create-info)))
    (we.dbg:msg :app "create shader module ~a in device ~a~%" module device)
    module))

(defun create-shader-stage (sys path stage
			    &aux
			      (module (create-shader-module sys path)))
  (vk:make-pipeline-shader-stage-create-info
   :name "main"
   :stage stage
   :module module))

(defun destroy-shader-stages (sys shaders
			      &aux
				(device (get-device sys)))
  ;; destroy a list of shaders on device
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (mapcar (lambda (shader &aux (module (vk:module shader)))
       (we.dbg:msg :app "destroy shader module ~a in device ~a~%" module device)
       (vk:destroy-shader-module device module))
     shaders))
;; ------------------------------------------------------------------------------
(defun input-assembly-state (topology)
  "set the topology by define-gpipeline"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-input-assembly-state-create-info
   :topology topology
   :primitive-restart-enable nil))
;; ------------------------------------------------------------------------------
(defun tessellation-state ()
  "set the default value"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-tessellation-state-create-info))
;; ------------------------------------------------------------------------------
(defun viewport-state ()
  "use the dynamic viewport state so don't care"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-viewport-state-create-info          ;; use dynamic state don't care
   :scissors (list
	      (vk:make-rect-2d
	       :offset (vk:make-offset-2d)
	       :extent (vk:make-extent-2d)))
   :viewports (list
	       (vk:make-viewport))))
;; ------------------------------------------------------------------------------
(defun rasterization-state (polygon-mode
			    cull-mode
			    front-face
			    line-width)
  "set pipeline rasterization state, no support args for now"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-rasterization-state-create-info
   :depth-clamp-enable nil
   :rasterizer-discard-enable nil
   :polygon-mode polygon-mode
   :cull-mode cull-mode
   :front-face front-face
   :depth-bias-enable nil
   :depth-bias-constant-factor 0f0
   :depth-bias-clamp 0f0
   :depth-bias-slope-factor 0f0
   :line-width line-width))
;; ------------------------------------------------------------------------------
(defun multisample-state (rasterization-samples)
  "set pipeline multisample state, no support args for now"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-multisample-state-create-info
   :sample-shading-enable nil
   :rasterization-samples rasterization-samples
   :min-sample-shading 0f0
   :sample-mask nil
   :alpha-to-coverage-enable nil
   :alpha-to-one-enable nil))
;; ------------------------------------------------------------------------------
(defun depth-stencil-state ()
  "set pipeline depth stencil state, no support args for now"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-depth-stencil-state-create-info
   :depth-test-enable t
   :depth-write-enable t
   :depth-compare-op :less-or-equal
   :depth-bounds-test-enable nil
   :stencil-test-enable nil
   :front (vk:make-stencil-op-state
   	   :fail-op :keep
   	   :pass-op :keep
   	   :depth-fail-op :keep
   	   :compare-op :always
   	   :compare-mask 0
   	   :write-mask 0
   	   :reference 0)
   :back (vk:make-stencil-op-state
   	  :fail-op :keep
   	  :pass-op :keep
   	  :depth-fail-op :keep
   	  :compare-op :always
   	  :compare-mask 0
   	  :write-mask 0
   	  :reference 0)
   :min-depth-bounds 0.0
   :max-depth-bounds 1.0))
;; ------------------------------------------------------------------------------
(defun color-blend-state ()
  "set pipeline color blend state, no support args for now"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-color-blend-state-create-info
   :logic-op-enable nil
   :logic-op :no-op
   :attachments (list
		 (vk:make-pipeline-color-blend-attachment-state
		  :blend-enable nil
		  :src-color-blend-factor :zero
		  :dst-color-blend-factor :zero
		  :color-blend-op :add
		  :src-alpha-blend-factor :zero
		  :dst-alpha-blend-factor :zero
		  :alpha-blend-op :add
		  :color-write-mask '(:r :g :b :a)))
   :blend-constants #(1f0 1f0 1f0 1f0)))
;; ------------------------------------------------------------------------------
(defun dynamic-state ()
  "set pipeline dynamic state can not modify"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-dynamic-state-create-info
   :dynamic-states '(:viewport :scissor)))
;; ------------------------------------------------------------------------------
(defun get-value (list key val
		  &aux
		    (slist (assoc key list)))
  (if slist (second slist) val))

(defmacro defgpipeline (name () &body body)
  "
usage ->
(defgpipeline test-pipeline ()
  (:vertex vertex-shader-path)
  (:fragment fragment-shader-path)
  (:vertex-stage vertex-buffer-by-defbuffer-usage-vertex)
  (:topology :triangle-list)
  (:polygon-mode :fill)
  (:cull-mode :back)
  (:front-face :clockwise)
  (:line-width 1.0)
  (:rasterization-samples :1))
"
  (let* ((vert-shader (second (assoc :vertex body)))
	 (frag-shader (second (assoc :fragment body)))
	 ;; vertex input stage
	 (vertex-info (second (assoc :vertex-stage body)))
	 (vbind-info-fun (we.u:create-symbol vertex-info '-vertex-binding-info))
	 (vattr-info-fun (we.u:create-symbol vertex-info '-vertex-attribute-info))
	 ;; input assembly stage
	 (topology (get-value body :topology :triangle-list))
	 ;; rasterization stage
	 (polygon-mode (get-value body :polygon-mode :fill))
	 (cull-mode (get-value body :cull-mode :back))
	 (front-face (get-value body :front-face :clockwise))
	 (line-width (get-value body :line-width 1.0))
	 ;; multisample-state
	 (rasterization-samples (get-value body :rasterization-samples :1))
	 ;; functions 
	 (make-fun (we.u:create-symbol 'makeg- name))
	 (destroy-fun (we.u:create-symbol 'destroyg- name))
	 ;; macro
	 (with-mac (we.u:create-symbol 'withg- name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (defun ,make-fun (sys layout &optional (input-rate :vertex))
	 (let* ((shader-stage (list (create-shader-stage sys ,vert-shader :vertex)
				    (create-shader-stage sys ,frag-shader :fragment)))
		(create-info (vk:make-graphics-pipeline-create-info
			      :base-pipeline-handle nil
			      :base-pipeline-index 0
			      :stages shader-stage
			      :vertex-input-state (vk:make-pipeline-vertex-input-state-create-info
						   :vertex-binding-descriptions (,vbind-info-fun input-rate)
						   :vertex-attribute-descriptions (,vattr-info-fun))
			      :input-assembly-state (input-assembly-state ,topology)
			      :tessellation-state (tessellation-state)
			      :viewport-state (viewport-state)
			      :rasterization-state (rasterization-state ,polygon-mode
									,cull-mode
									,front-face
									,line-width)
			      :multisample-state (multisample-state ,rasterization-samples)
			      :depth-stencil-state (depth-stencil-state)
			      :color-blend-state (color-blend-state)
			      :dynamic-state (dynamic-state)
			      :render-pass (get-render-pass sys)
			      :subpass 0
			      :layout layout))
		(pipeline (check-result #'vk:create-graphics-pipelines (get-device sys) (list create-info) (get-pipeline-cache sys))))
	   (we.dbg:msg :app "~a: create graphics pipeline ~a~%" ',make-fun pipeline)
	   (destroy-shader-stages sys shader-stage)
	   ;; single pipeline
	   (first pipeline)))
       (defun ,destroy-fun (sys pipeline)
       	 (we.dbg:msg :app "~a: destroy graphics pipeline ~a~%" ',destroy-fun pipeline)
       	 (vk:destroy-pipeline (get-device sys) pipeline))
       (defmacro ,with-mac ((val sys layout input-rate) &body wbody)
       	 (let ((makeg (we.u:create-symbol 'makeg- ',name))
       	       (destroyg (we.u:create-symbol 'destroyg- ',name)))
       	   `(let ((,val (,makeg ,sys ,layout ,input-rate)))
       	      ,@wbody
       	      (,destroyg ,sys ,val)))))))

(defmacro withg-pipelines ((sys layout &rest args) &body body)
  (destructuring-bind (first-bindings . rest-bindings) args
    (let* ((bindings (first first-bindings))
	   (name (second first-bindings))
	   (with-mac (we.u:create-symbol 'withg- name))
	   (input-rate (if (third first-bindings) (third first-bindings) 1)))
      (typecase bindings
	(null `(progn ,@body))
	(atom `(,with-mac (,bindings ,sys ,layout ,input-rate)
		 ,(if rest-bindings
		      `(withg-pipelines (,sys ,layout ,@rest-bindings) ,@body)
		      `(progn ,@body))))))))

;; ------------------------------------------------------------------------------
(defmacro defpipeline-layout (name () &body body)
  "
usage ->
  (defpipeline-layout name ()
    (:type :uniform-buffer
     :count 1
     :flags :vertex
     :samplers nil)
    (:type :combined-image-sampler
     :count 1
     :flags :fragment))
usage export 
  makepl-*name*
  makedsl-*name*
  destroypl-*name*
  withpl-*name*
"
  (let* ((make-fun (we.u:create-symbol 'makepl- name))      ;; function used to create pipeline layout
	 (dsl-info-fun (we.u:create-symbol '%makedsl- name))  ;; function used to make descriptor create info
	 (make-dsl (we.u:create-symbol 'makedsl- name))       ;; function used to make descriptor set layout
	 (destroy-fun (we.u:create-symbol 'destroypl- name))
	 (with-mac (we.u:create-symbol 'withpl- name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (defun ,dsl-info-fun ()
	 (list ,@(loop :for i :from 0 :below (length body)
		       :for bd := (nth i body)
		       :collect `(vk:make-descriptor-set-layout-binding
				  :binding ,i
				  :descriptor-type ,(getf bd :type)
				  :descriptor-count ,(getf bd :count)
				  :stage-flags ,(getf bd :flags)
				  :immutable-samplers ,(getf bd :samplers)))))
       (defun ,make-dsl (sys)
	 (let* ((create-info (vk:make-descriptor-set-layout-create-info
			      :bindings (,dsl-info-fun)))
		(layout (vk:create-descriptor-set-layout (get-device sys) create-info)))
	   (we.dbg:msg :app "create descriptor set layout ~a~%" ',make-dsl layout)
	   layout))
       (defun ,make-fun (sys)
	 (let* ((dsl ,(if body `(list (,make-dsl sys)) nil))
		(layout (vk:create-pipeline-layout (get-device sys)
						   (vk:make-pipeline-layout-create-info
						    :set-layouts dsl
						    :push-constant-ranges nil))))
	   (we.dbg:msg :app "~a: create graphics pipeline layout ~a~%" ',make-fun layout)
	   (values layout dsl)))
       (defun ,destroy-fun (sys layout dsl)
	 (when dsl
	   (we.dbg:msg :app "~a: destroy descriptor layout ~a~%" ',destroy-fun dsl)
	   (mapcar #'(lambda (l)
		 (vk:destroy-descriptor-set-layout (get-device sys) l))
	      dsl))
	 (we.dbg:msg :app "~a: destroy graphics pipeline layout ~a~%" ',destroy-fun layout)
	 (vk:destroy-pipeline-layout (get-device sys) layout))
       (defmacro ,with-mac ((playout dsl sys) &body wbody)
       	 (let ((makeg (we.u:create-symbol 'makepl- ',name))
       	       (destroyg (we.u:create-symbol 'destroypl- ',name)))
       	   `(multiple-value-bind (,playout ,dsl) (,makeg ,sys)
	      ,@wbody
	      (,destroyg ,sys ,playout ,dsl)))))))
