(in-package :we.ctrl)

(defmacro with-we-init ((sys &key
			       (w 800)
			       (h 800)
			       (x 0)
			       (y 0)
			       (title "wild engine demo")
			       (format :r8g8b8a8-srgb))
			&body body)
  "we system initial"
  `(sdl2:with-init (:everything)
     (let ((,sys (we.vk:vk->init-all ,w ,h ,x ,y ,title ,format)))
       (unwind-protect ,@body)
       (we.vk:vk->destroy-all ,sys))))

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

(defmacro with-we-main-loop ((&key
				(keydown)
				(keyup)
				(mousemotion)) &body body)
  `(sdl2:with-event-loop (:method :poll)
     (:keydown (:keysym key)
	       (when ,keydown
     		 (funcall ,keydown key)))
     (:keyup (:keysym key)
	     (when ,keyup
     	       (funcall ,keyup key)))
     (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
     		   (when ,mousemotion
     		     (funcall ,mousemotion x y xrel yrel state)))
     (:idle () (progn ,@body))
     (:quit () t)))

