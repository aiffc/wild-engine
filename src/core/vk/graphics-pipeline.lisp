(in-package :%wild-engine.core.vulkan)

(defparameter *pipeline-hash* (make-hash-table))
;;(defparameter *descriptor-hash* (make-hash-table))

;; (defstruct graphhics-pipeline
;;   (handle nil)
;;   (layout nil)
;;   (layout-set nil))

(defun load-shader-module (path)
  (with-open-file (in path
		      :direction :input
		      :element-type '(unsigned-byte 32))
    (let ((shader-code (make-array 1024 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 32))))
      (loop :for b := (read-byte in nil nil)
	    :while b
	    :do (vector-push-extend b shader-code)
	    :finally (return (adjust-array shader-code (length shader-code)))))))

(defun create-shader-module (app path 
			     &aux
			       (chandle (%we.utils:app-handle app))
			       (device (%we.utils:device chandle)))
  (let* ((create-info (vk:make-shader-module-create-info
		       :code (load-shader-module path)))
	 (module (check-result #'vk:create-shader-module device create-info)))
    (%we.dbg:msg :app "create shader module ~a in device ~a~%" module device)
    module))

(defun create-shader-stage (app path stage
			    &aux
			      (module (create-shader-module app path)))
  (vk:make-pipeline-shader-stage-create-info
   :name "main"
   :stage stage
   :module module))

(defun destroy-shader-stages (app shaders
			      &aux
				(chandle (%we.utils:app-handle app))
				(device (%we.utils:device chandle)))
  ;; destroy a list of shaders on device
  (mapcar (lambda (shader &aux (module (vk:module shader)))
       (%we.dbg:msg :app "destroy shader module ~a in device ~a~%" module device)
       (vk:destroy-shader-module device module))
     shaders))
;; ------------------------------------------------------------------------------
;; (defun vertex-input-stage (binding attribute)
;;   "set the vertex input state can set it by define-vertex-stage"
;;   (declare (optimize (speed 3) (safety 0) (debug 0))
;; 	   (ignore binding attribute))
;;   (vk:make-pipeline-vertex-input-state-create-info
;;    :vertex-binding-descriptions nil
;;    :vertex-attribute-descriptions nil))
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
(defun rasterization-state (depth-clamp-enable
			    rasterizer-discard-enable
			    polygon-mode
			    cull-mode
			    front-face
			    depth-bias-enable
			    depth-bias-constant-factor
			    depth-bias-clamp
			    depth-bias-slope-factor
			    line-width)
  "set pipeline rasterization state, no support args for now"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-rasterization-state-create-info
   :depth-clamp-enable depth-clamp-enable
   :rasterizer-discard-enable rasterizer-discard-enable
   :polygon-mode polygon-mode
   :cull-mode cull-mode
   :front-face front-face
   :depth-bias-enable depth-bias-enable
   :depth-bias-constant-factor depth-bias-constant-factor
   :depth-bias-clamp depth-bias-clamp
   :depth-bias-slope-factor depth-bias-slope-factor
   :line-width line-width))
;; ------------------------------------------------------------------------------
(defun multisample-state (sample-shading-enable
			  rasterization-samples
			  min-sample-shading
			  sample-mask
			  alpha-to-coverage-enable
			  alpha-to-one-enable)
  "set pipeline multisample state, no support args for now"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-pipeline-multisample-state-create-info
   :sample-shading-enable sample-shading-enable
   :rasterization-samples rasterization-samples
   :min-sample-shading min-sample-shading
   :sample-mask sample-mask
   :alpha-to-coverage-enable alpha-to-coverage-enable
   :alpha-to-one-enable alpha-to-one-enable))
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
(defun graphics-pipeline-create-info (app
				      &key
					(topology :triangle-list)
					(depth-clamp-enable nil)
					(rasterizer-discard-enable nil)
					(polygon-mode nil)
					(cull-mode nil)
					(front-face nil)
					(depth-bias-enable nil)
					(depth-bias-constant-factor 0.0)
					(depth-bias-clamp 0.0)
					(depth-bias-slope-factor 0.0)
					(line-width 0.0)
					(rasterization-samples nil)
					(sample-shading-enable nil)
					(min-sample-shading 0.0)
					(sample-mask nil)
					(alpha-to-coverage-enable nil)
					(alpha-to-one-enable nil)
				      &aux
					(chandle (%we.utils:app-handle app))
					(render-pass (%we.utils:render-pass chandle)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-graphics-pipeline-create-info
   :base-pipeline-handle nil
   :base-pipeline-index 0
   :vertex-input-state (vertex-input-stage)    ;; defined in vertex-buffer.lisp
   :input-assembly-state (input-assembly-state topology)
   :tessellation-state (tessellation-state)
   :viewport-state (viewport-state)
   :rasterization-state (rasterization-state depth-clamp-enable
					     rasterizer-discard-enable
					     polygon-mode
					     cull-mode
					     front-face
					     depth-bias-enable
					     depth-bias-constant-factor
					     depth-bias-clamp
					     depth-bias-slope-factor
					     line-width)
   :multisample-state (multisample-state sample-shading-enable
					 rasterization-samples
					 min-sample-shading
					 sample-mask
					 alpha-to-coverage-enable
					 alpha-to-one-enable)
   :depth-stencil-state (depth-stencil-state)
   :color-blend-state (color-blend-state)
   :dynamic-state (dynamic-state)
   :render-pass render-pass
   :subpass 0))

(defun create-graphics-pipeline (app name shaders create-fun &optional (layout nil) (descriptor nil)
				 &aux
				   (chandle (%we.utils:app-handle app))
				   (device (%we.utils:device chandle))
				   (create-info (funcall create-fun app)))
  (multiple-value-bind (playout set-layout) (create-layout app layout)
    (setf (vk:stages create-info) shaders
	  (vk:layout create-info) playout)
    (let* ((pipeline (vk:create-graphics-pipelines device (list create-info)))
	   (descriptor-pool (create-descriptor-pool app descriptor))
	   (descriptor-sets (alloc-descriptor-sets app set-layout descriptor-pool)))
      (%we.dbg:msg :app "create graphics pipeline ~a~%" pipeline)
      (setf *current-pipeline* name)
      (push (list :name name
		  :pipeline (nth 0 pipeline)
		  :layout playout
		  :set-layout set-layout
		  :descriptor-pool descriptor-pool
		  :descriptor-sets descriptor-sets)
	    (gethash app *pipeline-hash*)))))

(defun slot-pipeline (app name &optional (slot-type :pipeline)
		      &aux
			(pipelines (gethash app *pipeline-hash*)))
  "get pipeline info ny name"
  (getf (find name pipelines
	      :key (lambda (u)
		     (getf u :name))
	      :test #'eql)
	slot-type))

(defun destroy-graphics-pipeline (app
				  &aux
				    (chandle (%we.utils:app-handle app))
				    (device (%we.utils:device chandle))
				    (pipelines (gethash app *pipeline-hash*)))
  (when pipelines
    (mapcar (lambda (pipeline)
	 (free-descriptor-sets app (getf pipeline :descriptor-sets) (getf pipeline :descriptor-pool))
	 (destroy-descriptor-pool app (getf pipeline :descriptor-pool))
	 (destroy-layout app (getf pipeline :layout) (getf pipeline :set-layout))
	 (%we.dbg:msg :app "destroy graphics pipeline ~a~%" pipeline)
	 (vk:destroy-pipeline device (getf pipeline :pipeline)))
       pipelines)
    (setf *current-pipeline* nil)
    (setf (gethash app *pipeline-hash*) nil)))

(defun get-gpipeline (app name
		      &aux
			(pipelines (gethash app *pipeline-hash*))
			(pipeline (find name pipelines :key (lambda (lst)
							      (getf lst :name)))))
  (if pipeline
      (getf pipeline :pipeline)
      (error "can not get pipeline")))

