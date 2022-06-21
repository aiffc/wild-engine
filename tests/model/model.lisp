(in-package :we.model)

(%we.dbg:dbg-untrace)

(defparameter *run-path* (concatenate 'string
				      (namestring (asdf:system-relative-pathname :wild-engine "tests/"))
				      "model/"))
(defparameter *texture-path* (concatenate 'string *run-path* "spot/spot_texture.png"))
(defparameter *model-path* (concatenate 'string *run-path* "spot/spot_triangulated_good.obj"))

(we.api:define-shader-stage model-shader ()  ;; just use image shader
  (we.tu:*model-vert* :vertex)
  (we.tu:*model-frag* :fragment))

(we.api:define-graphics-pipeline model-layout (model-shader)
  (:layout
   ((:uniform-buffer
      :name ubo
      :binding 0
      :struct ((:type :mat4
		:accessor model
		:initform (m4:rotation-from-axis-angle (v3:make 0.0 1.0 0.0) -45.0))
	       (:type :mat4
		:accessor view
		:initform (m4:look-at (v3:make 1.0 1.0 0.0)
				      (v3:make 2.0 2.0 2.0)
				      (v3:make 0.0 0.0 0.0)))
	       (:type :mat4
		:accessor proj
		:initform (rtg-math.projection:perspective 600.0 600.0 0.1 10.0 45.0))))
     (:texture
      :name test-texture
      :binding 1
      :path *texture-path*)))
  (:assembly
   ;;:topology :triangle-list
   :topology :point-list)
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

(we.api:define-model test-model *model-path*)

(defun model-test (&key
		     (x :centered)
		     (y :centered)
		     (w 600)
		     (h 600)
		     (title "windows test")
		     (uangle 0.0))
  (we.api:with-app (app :win-title title
			:win-x x
			:win-y y
			:win-w w
			:win-h h
			:pipefun (model-layout))
    (we.u:with-mvalues (((vbuf ibuf icount) (createm-test-model app)))
      (we.api:with-main-loop ()
	(we.api:with-render (app cmd
			     :clear-color #(0.0 0.0 0.0 1.0)
			     :update-funs (ubo))
	  (uset-ubo 'model (m4:rotation-from-axis-angle (v3:make 1.0 1.0 0.0) (incf uangle 0.01)))
	  (we.api:bind-gpipeline app 'model-layout cmd)
	  (we.api:set-viewport cmd) 
	  (we.api:set-scissor cmd)
	  (we.api:set-vertex cmd vbuf)
	  (we.api:set-index cmd ibuf)
	  (we.api:draw cmd :icount icount :index-p t))))))

