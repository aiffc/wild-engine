(in-package :%wild-engine.core.vk)

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

(defparameter *uniform-buffer-size* 0)
(defparameter *texture-buffer-size* 0)

(defun check-result (fn &rest args)
  "check-result -> function function arguments"
  (multiple-value-bind (handle result) (apply fn args)
    (cond ((or (member result +success-code+) (member handle +success-code+))
	   (we.dbg:msg :vk "do fn ~a success code: ~a~%" fn (or result handle)))
	  ((member result +error-code+)
	   (error 'vk-failed-info :fn fn :result-code result))
	  (t (we.dbg:msg :vk "unknow error~a ~a ~a~%" fn handle result)))
    (values handle result)))

(defun find-memory (sys type properties)
  "function used to find memory by memory info and gpu properties"
  (let ((memory-types (vk:memory-types (get-gpu-memory-properties sys))))
    (loop :for i :from 0 :below (vk:memory-type-count (get-gpu-memory-properties sys))
	  :for shift := (ash 1 i)
	  :for mem-p := (vk:property-flags (nth i memory-types))
	  :do
	     (we.dbg:msg :app "~a ~a ~a~%" i shift mem-p)
	  :when (and (not (zerop (logand type shift)))
		     (or (find properties mem-p :test #'eql) (equal properties mem-p)))
	    :return i)))

;; -----------------------------------------------------------------------------------
(defun create-transfer-pool (sys)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((cpt (check-result #'vk:create-command-pool
			    (get-device sys)
			    (vk:make-command-pool-create-info
			     :flags :reset-command-buffer
			     :queue-family-index (get-device-tqindex sys))))
	 (cbt (check-result #'vk:allocate-command-buffers
			    (get-device sys)
			    (vk:make-command-buffer-allocate-info
			     :command-pool cpt
			     :level :primary
			     :command-buffer-count 1))))
    ;; just use single
    (values cpt (first cbt))))

(defun destroy-transfer-pool (sys cpt cbt)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (we.dbg:msg :app "~4tdestroy transfer~%" cbt cpt)
  (we.dbg:msg :app "~4tfree cmds [~a] in pool [~a]~%" cbt cpt)
  (vk:free-command-buffers (get-device sys) cpt (list cbt))
  (we.dbg:msg :app "~4tdestroy command-pool [~a] in [~a]~%" cpt (get-device sys))
  (vk:destroy-command-pool (get-device sys) cpt))

(defun begin-transfer (cmd)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (we.dbg:msg :app "~%enter begin tcmd [~a]->~%" cmd)
  (vk:begin-command-buffer cmd
			   (vk:make-command-buffer-begin-info
			    :flags :one-time-submit)))

(defun end-transfer (sys cmd)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((submit-info (vk:make-submit-info
		      :command-buffers (list cmd)))
	(fence (check-result #'vk:create-fence (get-device sys) (vk:make-fence-create-info))))
    (we.dbg:msg :app "~%enter end tcmd [~a]->~%" cmd)
    (vk:end-command-buffer cmd)
    (vk:queue-submit (get-device-tqueue sys) (list submit-info) fence)
    ;;(vk:queue-wait-idle (get-device-tqueue sys))
    (vk:wait-for-fences (get-device sys) (list fence) t #xffffffff)
    (vk:destroy-fence (get-device sys) fence)))

(defmacro with-transfer-cmd ((sys cmd) &body body)
  (let ((pool (gensym "transfer-pool")))
    `(multiple-value-bind (,pool ,cmd) (create-transfer-pool ,sys)
       (begin-transfer ,cmd)
       ,@body
       (end-transfer ,sys ,cmd)
       (destroy-transfer-pool ,sys ,pool ,cmd))))
;; -----------------------------------------------------------------------------------

