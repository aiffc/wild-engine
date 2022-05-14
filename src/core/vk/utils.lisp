(in-package :%wild-engine.core.vulkan)

(defparameter *buffer-hash* (make-hash-table))

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

(defun map-memory (app mem obj size
		   &aux
		     (chandle (%we.utils:app-handle app))
		     (device (%we.utils:device chandle)))
  "function used to map memory"
  (cffi:with-foreign-object (data :pointer)
    (vk:map-memory device mem 0 size data)
    (we.u:memcpy (cffi:mem-aref data :pointer) obj size)
    (vk:unmap-memory device mem)))

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

(defun begin-transfer (cmd)
  (%we.dbg:msg :app "~%enter begin tcmd [~a]->~%" cmd)
  (vk:begin-command-buffer cmd
			   (vk:make-command-buffer-begin-info
			    :flags :one-time-submit)))

(defun end-transfer (app cmd
		     &aux
		       (chandle (%we.utils:app-handle app))
		       (tqueue (nth 0 (%we.utils:device-tqueues chandle))))
  (let ((submit-info (vk:make-submit-info
		      :command-buffers (list cmd))))
    (%we.dbg:msg :app "~%enter end tcmd [~a]->~%" cmd)
    (vk:end-command-buffer cmd)
    (vk:queue-submit tqueue (list submit-info))
    (vk:queue-wait-idle tqueue)))

(defun create-transfer-pool (device tfindex)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((cpt (check-result #'vk:create-command-pool
			    device
			    (vk:make-command-pool-create-info
			     :flags :reset-command-buffer
			     :queue-family-index tfindex)))
	 (cbt (check-result #'vk:allocate-command-buffers
			    device
			    (vk:make-command-buffer-allocate-info
			     :command-pool cpt
			     :level :primary
			     :command-buffer-count 1))))
    (values cpt cbt)))

(defun destroy-transfer-pool (device cpt cbt)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%we.dbg:msg :app "~4tdestroy transfer~%" cbt cpt)
  (%we.dbg:msg :app "~4tfree cmds [~a] in pool [~a]~%" cbt cpt)
  (vk:free-command-buffers device cpt cbt)
  (%we.dbg:msg :app "~4tdestroy command-pool [~a] in [~a]~%" cpt device)
  (vk:destroy-command-pool device cpt))

(defmacro with-transfer-cmd ((app cmd) &body body)
  (let ((pool (gensym "transfer-pool"))
	(cmds (gensym "tcmds")))
    `(let* ((chandle (%we.utils:app-handle app))
	    (device (%we.utils:device chandle))
	    (tfindex (%we.utils:device-tqueue-family-index chandle)))
       (multiple-value-bind (,pool ,cmds) (create-transfer-pool device tfindex)
	 (let ((,cmd (nth 0 ,cmds)))
	   (begin-transfer ,cmd)
	   ,@body
	   (end-transfer ,app ,cmd))
	 (destroy-transfer-pool device ,pool ,cmds)))))

(defun create-buffer (app size usage properties &optional (stage-p nil)
		      &aux
			(chandle (%we.utils:app-handle app))
			(device (%we.utils:device chandle))
			(mem-info (%we.utils:gpu-memory-infos chandle)))
  "function for create buffer and allocate memory for the buffer"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((create-info (vk:make-buffer-create-info
		       :size size
		       :usage usage
		       :sharing-mode :exclusive))
	 (buffer (check-result #'vk:create-buffer device create-info))
	 (req (vk:get-buffer-memory-requirements device buffer))
	 (mem-size (vk:size req))
	 (type-index (find-memory (vk:memory-type-bits req)
				  properties
				  mem-info))
	 (alloc-info (vk:make-memory-allocate-info
		      :allocation-size mem-size
		      :memory-type-index type-index))
	 (memory (check-result #'vk:allocate-memory device alloc-info)))
    (vk:bind-buffer-memory device buffer memory 0)
    ;; stage buffer and memory free in self function
    (unless stage-p
      (push (list :buffer buffer
		  :memory memory)
	    (gethash app *buffer-hash*)))
    (values buffer memory)))

(defun free-memory (app memory
		    &aux
		      (chandle (%we.utils:app-handle app))
		      (device (%we.utils:device chandle)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%we.dbg:msg :app "free memory ~a~%" memory)
  (vk:free-memory device memory))

(defun destroy-buffer (app buffer
		       &aux
			 (chandle (%we.utils:app-handle app))
			 (device (%we.utils:device chandle)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%we.dbg:msg :app "destroy buffer ~a~%" buffer)
  (vk:destroy-buffer device buffer))

(defun free-buffer (app
		    &aux
		      (buffers (gethash app *buffer-hash*)))
  "function used to destroy buffer and free memory"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when buffers
    (mapcar (lambda (lst)
	 (let ((buffer (getf lst :buffer))
	       (memory (getf lst :memory)))
	   (when memory
	     (free-memory app memory))
	   (when buffer
	     (destroy-buffer app buffer))))
       buffers)
    (setf (gethash app *buffer-hash*) nil)))

(defmacro with-stage-buffer ((sbuffer smemory app size usage properties) &body body)
  `(multiple-value-bind (,sbuffer ,smemory)
       (create-buffer ,app ,size ,usage ,properties t)
     ,@body))

(defun copy-buffer (app src dst size)
  "function used to copy buffer from src to dst"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((region (vk:make-buffer-copy
		 :src-offset 0
		 :dst-offset 0
		 :size size)))
    (with-transfer-cmd (app cmd)
      (vk:cmd-copy-buffer cmd src dst (list region)))))

(defmacro define-buffer-fun (name (usage vsize) &optional (doc nil))
  `(defun ,name (app buf size
		 &aux
		   (buffer-size (* ,vsize size)))
     ,doc
     (with-stage-buffer (sbuffer smemory app buffer-size :transfer-src '(:host-visible :host-coherent))
       (map-memory app smemory buf buffer-size)
       (multiple-value-bind (buffer)
	   (create-buffer app buffer-size '(:transfer-dst ,usage) :device-local)
	 (copy-buffer app sbuffer buffer buffer-size)
	 (free-memory app smemory)
	 (destroy-buffer app sbuffer)
	 (values buffer size)))))
