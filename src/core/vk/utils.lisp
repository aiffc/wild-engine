(in-package :%wild-engine.core.vulkan)

(defparameter +success-code+
  '(:success
    :not-ready
    :timeout
    :event-set
    :event-reset
    :incomplete
    :suboptimal-khr
    :thread-idle-khr
    :thread-done-khr
    :operation-deferred-khr
    :operation-not-deferred-khr
    :pipeline-compile-required-ext)
  "success list")

(defparameter +error-code+
  '(:error-out-of-host-memory
    :error-out-of-device-memory
    :error-initialization-failed
    :error-device-lost
    :error-memory-map-failed
    :error-layer-not-present
    :error-extension-not-present
    :error-feature-not-present
    :error-incompatible-driver
    :error-too-many-objects
    :error-format-not-supported
    :error-fragmented-pool
    :error-surface-lost-khr
    :error-native-window-in-use-khr
    ;; :error-out-of-date-khr
    :error-incompatible-display-khr
    :error-invalid-shader-nv
    :error-out-of-pool-memory
    :error-fragmented-pool
    :error-invalid-external-handle
    :error-fragmentation
    :error-invalid-opaque-capture-address
    :error-full-screen-exclusive-mode-lost-ext
    :error-unknown)
  "error list")

(define-condition vk-failed-info (error)
  ((fn :initarg :fn)
   (result-code :initarg :result-code))
  (:report (lambda (obj stream)
	     (with-slots (fn result-code) obj
	       (format stream "do fn [~a] failed retcode[~a]~%" fn result-code)))))

(defun check-result (fn &rest args)
  "check-result -> function function arguments"
  (multiple-value-bind (handle result) (apply fn args)
    (cond ((or (member result +success-code+) (member handle +success-code+))
	   (%we.dbg:msg :vk "do fn ~a success code: ~a~%" fn (or result handle)))
	  ((member result +error-code+)
	   (error 'vk-failed-info :fn fn :result-code result))
	  (t (%we.dbg:msg :vk "unknow error~a ~a ~a~%" fn handle result)))
    (values handle result)))

(defun find-memory (type properties mem-info)
  "function used to find memory by memory info and gpu properties"
  (let ((memory-types (vk:memory-types mem-info)))
    (loop :for i :from 0 :below (vk:memory-type-count mem-info)
	  :for shift := (ash 1 i)
	  :for mem-p := (vk:property-flags (nth i memory-types))
	  :do
	     (%we.dbg:msg :app "~a ~a ~a~%" i shift mem-p)
	  :when (and (not (zerop (logand type shift)))
		     (or (find properties mem-p :test #'eql) (equal properties mem-p)))
	    :return i)))
