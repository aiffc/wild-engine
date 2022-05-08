(in-package :wild-engine.api)

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
body like arguments are all optioanal
(define-graphics-pipeline demo (shaders)
  (:assembly
    :topology :triangle-list)
  (:rasterization 
    :clamp nil
    :discard nil
    :polygon-mode :fill
    :cull-mode :back
    :front-face :clockwise
    :bias nil
    :bias-constant-factor 0f0
    :bias-clamp 0f0
    :bias-slope-factor 0f0
    :line-width 1.0)
  (:multiple-sample 
    :sample-shading nil
    :rasterization-samples :1
    :min 0f0
    :mask nil
    :atc nil
    :ato nil))
"
  (let ((gpipeline-create-info-fun (we.u:create-symbol 'g- name '-create-info))
	(assembly-args (parser-assembly-args (assoc :assembly body)))
	(rasterization-args (parser-rasterization-args (assoc :rasterization body)))
	(multiple-sample-args (parser-multiple-sample-args (assoc :multiple-sample body))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (defun ,gpipeline-create-info-fun (app)
	 (%we.vk:graphics-pipeline-create-info app
					       ,@assembly-args
					       ,@rasterization-args
					       ,@multiple-sample-args)))))
