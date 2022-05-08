(in-package #:%wild-engine.debug)

;; support debug arg
;; :vk :app :app-high

(defparameter *trace-layer* nil)

(defun msg (layer str &rest args)
  (when (and *trace-layer* (member layer *trace-layer*))
    (fresh-line *debug-io*)
    (apply #'format *debug-io* str args)))

(defun vk-debug-p ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (member :vk *trace-layer*))

(defun dbg-trace (&rest args)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (setf *trace-layer* (union args *trace-layer*)))

(defun dbg-untrace (&rest args)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if args
      (setf *trace-layer* (set-difference *trace-layer* args))
      (setf *trace-layer* nil)))
