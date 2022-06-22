(in-package :we.triangle)

(%we.dbg:dbg-trace :app :vk)

(we.api:define-shader-stage triangle-shader ()
  (we.tu:*triangle-vert* :vertex)
  (we.tu:*triangle-frag* :fragment))

(we.api:define-graphics-pipeline triangle (triangle-shader)
  (:layout  ())
  (:assembly
   :topology :triangle-list)
  (:rasterization 
   :polygon-mode :fill
   :cull-mode :back
   :front-face :clockwise
   :line-width 1.0)
  (:multiple-sample 
   :rasterization-samples :1))

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
	(we.api:bind-gpipeline app 'triangle cmd)
	;; just use default value
	(we.api:set-viewport cmd)
	(we.api:set-scissor cmd)
	(we.api:draw cmd :vcount 3)))))
