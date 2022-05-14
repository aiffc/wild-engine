(in-package :we.win-test)

(%we.dbg:dbg-trace :app)

(defun win-test (&key
		   (x :centered)
		   (y :centered)
		   (w 600)
		   (h 600)
		   (title "windows test"))
  (we.api:with-app (app :win-title title
			:win-x x
			:win-y y
			:win-w w
			:win-h h)
    (we.api:with-main-loop ()
      (we.api:with-render (app cmd :clear-color #(1.0 0.0 0.0 1.0))))))
