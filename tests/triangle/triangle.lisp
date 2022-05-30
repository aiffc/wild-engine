(in-package :we.triangle)

(%we.dbg:dbg-trace :app)

(we.api:define-shader-stage triangle-shader ()
  (we.tu:*triangle-vert* :vertex)
  (we.tu:*triangle-frag* :fragment))

(we.api:define-layout triangle () ())

(we.api:define-graphics-pipeline triangle (triangle-shader triangle)
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

(defun triangle (&key
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
			:pipefun (triangle))
    (we.api:with-main-loop ()
      (we.api:with-render (app cmd :clear-color #(0.0 0.0 0.0 1.0))
	(we.api:bind-graphics-pipeline app cmd #'gslot-triangle)
	;; just use default value
	(we.api:set-viewport cmd)
	(we.api:set-scissor cmd)
	(we.api:draw cmd :vcount 3)))))
