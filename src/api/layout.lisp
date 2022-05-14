(in-package :wild-engine.api)

(defmacro define-layout (name (&rest body))
  "ready to do"
  (let ((layout-fun (we.u:create-symbol 'layout- name)))
    `(progn
       (eval-when (:compile-toplevel :execute :load-toplevel))
       (defun ,layout-fun ()
	 (%we.vk:layout-create-info ,@body)))))
