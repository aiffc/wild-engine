(in-package :wild-engine.api)

(defun create-app (app app-handle &rest args)
  (%we.utils:make-app app app-handle args))

(defmacro with-app ((app &rest args) &body body)
  (let ((app-sym (gensym "app"))
	(pipeline-init-fun (mapcar (lambda (name)
				(we.u:create-symbol 'createg- name))
			      (getf args :pipefun))))
    (setf (getf args :pipefun) `',pipeline-init-fun)
    `(sdl2:with-init (:everything)
       (let ((,app-sym (make-instance '%we.utils:app))
	     (,app (gensym "tapp")))
	 (create-app ,app ,app-sym ,@args)
	 (unwind-protect ,@body)
	 (%we.utils:destroy-app ,app ,app-sym)))))

(defmacro defkey-down (name (key) &body body)
  (let ((ksym (gensym)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (defun ,name (,ksym)
	 (let ((,key (sdl2:scancode-value ,ksym)))
	   ,@body)))))

(defmacro defkey-up (name (key) &body body)
  (let ((ksym (gensym)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (defun ,name (,ksym)
	 (let ((,key (sdl2:scancode-value ,ksym)))
	   ,@body)))))

(defmacro defmouse (name (x y xrel yrel state) &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute))
     (defun ,name (,x ,y ,xrel ,yrel ,state)
       ,@body)))

(defmacro with-main-loop ((&key
			     (keydown )
			     (keyup )
			     (mousemotion )) &body body)
  "main loop for app"
  `(sdl2:with-event-loop (:method :poll)
     (:keydown (:keysym key) (when ,keydown
     			       (funcall ,keydown key)))
     (:keyup (:keysym key) (when ,keyup
     			     (funcall ,keyup key)))
     (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
     		   (when ,mousemotion
     		     (funcall ,mousemotion x y xrel yrel state)))
     (:idle () (progn ,@body))
     (:quit () t)))

(defmacro with-render ((app cmd &key
				  (x 0)
				  (y 0)
				  (width 600)
				  (height 600)
				  (clear-color #(1.0 1.0 1.0 1.0))
				  (update-funs nil)) &body body)
  (let ((update-funs (mapcar (lambda (name)
			  (we.u:create-symbol 'uupdate- name))
			update-funs)))
    `(%we.vk:with-gcmd (,app ,cmd ,x ,y ,width ,height ,clear-color ',update-funs)
       ,@body)))
