(in-package :we.index)

(%we.dbg:dbg-untrace)

(we.api:define-shader-stage index-shader ()
  (we.tu:*vertex-vert* :vertex)         ;; just use vertex shader
  (we.tu:*vertex-frag* :fragment))

(we.api:define-layout index () ())

(we.api:define-graphics-pipeline index (index-shader index)
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

(we.api:define-vertex index ()
  (we.api:make-vertex :v #(-1.0 -1.0 0.0) :vt #(1.0 1.0 1.0))
  (we.api:make-vertex :v #(1.0 -1.0 0.0) :vt #(0.0 1.0 1.0))
  (we.api:make-vertex :v #(1.0 1.0 0.0) :vt #(1.0 0.0 1.0))
  (we.api:make-vertex :v #(-1.0 1.0 0.0) :vt #(1.0 1.0 0.0)))

(we.api:define-index index ()
  0 1 2 2 3 0)

(defun index (&key
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
			:pipefun (index))
    (we.u:with-mvalues (((vbuf) (createv-index app))
			((ibuf icount) (createi-index app)))
      (we.api:with-main-loop ()
	(we.api:with-render (app cmd :clear-color #(0.0 0.0 0.0 1.0))
	  (we.api:bind-gpipeline app 'index cmd)
	  (we.api:set-viewport cmd)
	  (we.api:set-scissor cmd)
	  (we.api:set-vertex cmd vbuf)
	  (we.api:set-index cmd ibuf)
	  (we.api:draw cmd :icount icount :index-p t))))))


