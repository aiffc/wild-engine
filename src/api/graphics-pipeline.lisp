(in-package :wild-engine.api)

(defmacro define-shader-stage (name () &body body)
  (let ((fun (we.u:create-symbol name '-shader-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (defun ,fun (app)
	 (mapcar (lambda (arg)
	      (apply #'%we.vk:create-shader-stage (list app
						       (eval (first arg))
						       (second arg))))
	    ',body)))))

(defmacro with-shaders ((shaders app name) &body body)
  (let ((fun (we.u:create-symbol name '-shader-list)))
    `(let ((,shaders (funcall #',fun ,app)))
       ,@body
       (%we.vk:destroy-shader-stages ,app ,shaders))))

(defun parser-assembly-args (args)
  "internal function used to parser assembly args for graphics pipeline"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((rargs (rest args)))
    (list :topology (we.u:set-value rargs :topology :triangle-list))))

(defun parser-rasterization-args (args)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((rargs (rest args)))
    (list :depth-clamp-enable (we.u:set-value rargs :clamp nil)
	  :rasterizer-discard-enable (we.u:set-value rargs :discard nil)
	  :polygon-mode (we.u:set-value rargs :polygon-mode :fill)
	  :cull-mode (we.u:set-value rargs :cull-mode :back)
	  :front-face (we.u:set-value rargs :front-face :clockwise)
	  :depth-bias-enable (we.u:set-value rargs :bias nil)
	  :depth-bias-constant-factor (we.u:set-value rargs :bias-constant-factor 0f0)
	  :depth-bias-clamp (we.u:set-value rargs :bias-clamp 0f0)
	  :depth-bias-slope-factor (we.u:set-value rargs :bias-slope-factor 0f0)
	  :line-width (we.u:set-value rargs :line-width 1.0))))

(defun parser-multiple-sample-args (args)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((rargs (rest args)))
    (list :sample-shading-enable (we.u:set-value rargs :sample-shading nil)
	  :rasterization-samples (we.u:set-value rargs :rasterization-samples :1)
	  :min-sample-shading (we.u:set-value rargs :min 0f0)
	  :sample-mask (we.u:set-value rargs :mask nil)
	  :alpha-to-coverage-enable (we.u:set-value rargs :atc nil)
	  :alpha-to-one-enable (we.u:set-value rargs :ato nil))))

(defmacro define-graphics-pipeline (name (shaders) &body body)
  "
;; just support single pipeline now
;; body like arguments are all optioanal
;; (define-graphics-pipeline demo (shaders)
;;   (:assembly
;;     :topology :triangle-list)
;;   (:rasterization 
;;     :clamp nil
;;     :discard nil
;;     :polygon-mode :fill
;;     :cull-mode :back
;;     :front-face :clockwise
;;     :bias nil
;;     :bias-constant-factor 0f0
;;     :bias-clamp 0f0
;;     :bias-slope-factor 0f0
;;     :line-width 1.0)
;;   (:multiple-sample 
;;     :sample-shading nil
;;     :rasterization-samples :1
;;     :min 0f0
;;     :mask nil
;;     :atc nil
;;     :ato nil))
"
  (let ((gpipeline-create-info-fun (we.u:create-symbol 'g- name '-create-info))
	(gpipeline-create-fun (we.u:create-symbol 'createg- name))
	(slot-fun (we.u:create-symbol 'gslot- name))
	(layout-fun (we.u:create-symbol 'layout- name))
	(layout-body (second (assoc :layout body)))
	(descriptor-fun (we.u:create-symbol 'descriptor- name))
	
	(assembly-args (parser-assembly-args (assoc :assembly body)))
	(rasterization-args (parser-rasterization-args (assoc :rasterization body)))
	(multiple-sample-args (parser-multiple-sample-args (assoc :multiple-sample body)))
	(shader-sym (gensym "shader")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (define-layout ,name () ,layout-body)
       (defun ,gpipeline-create-info-fun (app)
	 (%we.vk:graphics-pipeline-create-info app
					       ,@assembly-args
					       ,@rasterization-args
					       ,@multiple-sample-args))
       (defun ,slot-fun (app)
	 (%we.vk:get-gpipeline app ',name))
       ,(let ((uniform-funs (gethash name *uniform-hash*))
	      (texture-funs (gethash name *texture-hash*))
	      (texture-info-funs (gethash name *texture-info-hash*)))
	  `(defun ,gpipeline-create-fun (app)
	     (with-shaders (,shader-sym app ,shaders)
	       (mapc #'(lambda (fun)
		     (funcall fun app))
		  ',uniform-funs)
	       (mapc #'(lambda (fun)
		     (funcall fun app))
		  ',texture-funs)
	       ;;(setf (gethash layout *uniform-hash*) nil)
	       (%we.vk:create-graphics-pipeline app ',name ,shader-sym #',gpipeline-create-info-fun #',layout-fun #',descriptor-fun ',texture-info-funs)))))))

;; (defmacro bind-gpipeline (app cmd name)
;;   (let ((slot-fun (we.u:create-symbol 'gslot- name)))
;;     `(vk:cmd-bind-pipeline ,cmd :graphics (funcall #',slot-fun ,app))))

(defun bind-gpipeline (app name cmd)
  (%we.vk:bind-pipeline app name cmd))

