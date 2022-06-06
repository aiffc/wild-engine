(in-package :we.vertex)

;;(%we.dbg:dbg-trace :app :vk)
(%we.dbg:dbg-untrace)

(we.api:define-shader-stage vertex-shader ()
  (we.tu:*vertex-vert* :vertex)
  (we.tu:*vertex-frag* :fragment))

(we.api:define-layout vertex () ())

(we.api:define-graphics-pipeline vertex (vertex-shader vertex)
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

(we.api:define-vertex vertex ()
  (we.api:make-vertex :v #(0.0 -0.5 0.0) :vt #(1.0 1.0 0.0))
  (we.api:make-vertex :v #(0.5 0.5 0.0) :vt #(0.0 1.0 1.0))
  (we.api:make-vertex :v #(-0.5 0.5 0.0) :vt #(1.0 0.0 1.0)))

(defun vertex (&key
		 (x :centered)
		 (y :centered)
		 (w 600)
		 (h 600)
		 (title "windows test"))
  (we.api:with-app (app :win-title title
			:win-x x
			:win-y y
			:win-w w
			:win-h h
			:pipefun (vertex))
    (multiple-value-bind (vbuffer vcount) (createv-vertex app)
      (we.api:with-main-loop ()
	(we.api:with-render (app cmd :clear-color #(0.0 0.0 0.0 1.0))
	  (we.api:bind-gpipeline app 'vertex cmd)
	  (we.api:set-viewport cmd)
	  (we.api:set-scissor cmd)
	  (we.api:set-vertex cmd vbuffer)
	  (we.api:draw cmd :vcount vcount))))))

