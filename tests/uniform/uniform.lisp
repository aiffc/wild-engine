(in-package :we.uniform)

;;(%we.dbg:dbg-trace :app :vk)
(%we.dbg:dbg-untrace)

(we.api:define-shader-stage uniform-shader ()
  (we.tu:*uniform-vert* :vertex)
  (we.tu:*uniform-frag* :fragment))

(we.api:define-layout uniform ()
    ((:uniform-buffer
      :name ubo
      :binding 0
      :struct ((:type :mat4
		:accessor model
		:initform (m4:rotation-from-axis-angle (v3:make 0.0 0.0 0.0) 45.0))
	       (:type :mat4
		:accessor view
		:initform (m4:look-at (v3:make 0.0 0.0 1.0)
				      (v3:make 2.0 2.0 2.0)
				      (v3:make 0.0 0.0 0.0)))
	       (:type :mat4
		:accessor proj
		:initform (rtg-math.projection:perspective 600.0 600.0 0.1 10.0 -45.0))))))

(we.api:define-graphics-pipeline uniform (uniform-shader uniform)
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

(we.api:define-vertex uniform-vertex-data ()
  (we.api:make-vertex :v #(-1.0 -1.0 0.0) :vt #(1.0 1.0 1.0))
  (we.api:make-vertex :v #(1.0 -1.0 0.0) :vt #(0.0 1.0 1.0))
  (we.api:make-vertex :v #(1.0 1.0 0.0) :vt #(1.0 0.0 1.0))
  (we.api:make-vertex :v #(-1.0 1.0 0.0) :vt #(1.0 1.0 0.0)))

(we.api:define-index uniform-index-data ()
  0 1 2 2 3 0)

(defun uniform (&key
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
			:pipefun (uniform))
    (we.u:with-mvalues (((vbuf) (createv-uniform-vertex-data app))
			((ibuf icount) (createi-uniform-index-data app)))
      (we.api:with-main-loop ()
	(we.api:with-render (app cmd
			     :clear-color #(0.0 0.0 0.0 1.0)
			     :update-funs (ubo))
	  (uset-ubo 'model (m4:rotation-from-axis-angle (v3:make 0.0 0.0 1.0) (incf uangle 0.01)))
	  (we.api:bind-gpipeline app 'uniform cmd)
	  (we.api:set-viewport cmd)
	  (we.api:set-scissor cmd)
	  (we.api:set-vertex cmd vbuf)
	  (we.api:set-index cmd ibuf)
	  (we.api:draw cmd :icount icount :index-p t))))))
