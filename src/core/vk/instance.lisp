(in-package :%wild-engine.core.vk)

(cffi:defcallback debug-callback %vk:bool32 ((message-severity %vk:debug-utils-message-severity-flag-bits-ext)
					     (message-type %vk:debug-utils-message-type-flags-ext)
					     (call-back-data %vk:debug-utils-messenger-callback-data-ext)
					     (p-user-data :pointer))
  (we.dbg:msg :vk "message-severity: ~a~%" message-severity)
  (we.dbg:msg :vk "message-type: ~a~%" message-type)
  (we.dbg:msg :vk "debug message: ~a~%" (vk:message call-back-data))
  (when p-user-data
    (we.dbg:msg :vk "user-dataL: ~a~%" (cffi:foreign-string-to-lisp p-user-data)))
  (we.dbg:msg :vk "------------------------------------------------------------------------------~%")
  nil)

(defun create-debug-report-callback (instance create-info)
  (check-result #'vk:create-debug-utils-messenger-ext instance create-info))

(defun get-all-instance-layers ()
  "get all instance layers"
  (remove-duplicates (mapcar #'vk:layer-name (vk:enumerate-instance-layer-properties))
		     :test #'string=))

(defun get-all-instance-extensions ()
  "get all instance extensions"
  (remove-duplicates  (append (mapcar #'vk:extension-name
				      (vk:enumerate-instance-extension-properties)) 
			      (apply #'append
				     (mapcar (lambda (layer)
						 (mapcar #'vk:extension-name
							 (vk:enumerate-instance-extension-properties layer)))
					     (get-all-instance-layers))))
		      :test #'string=))

(defun instance-create-info ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-instance-create-info
   :application-info (vk:make-application-info
		      :application-name "wild engine"
		      :application-version (vk:make-api-version 0 0 1 0)
		      :engine-name "wild engine"
		      :engine-version (vk:make-api-version 0 0 1 0)
		      :api-version (vk:make-api-version 1 2 0 0))
   :enabled-layer-names nil
   :enabled-extension-names nil))

(defun vk->init-instance (sys &aux (create-info (instance-create-info)))
  (we.dbg:msg :app "create instance: -> ~%")
  (let ((debug-report-create-info (vk:make-debug-utils-messenger-create-info-ext
				   :message-type '(:validation :general :performance)
				   :message-severity '(:warning :error :info)
				   :pfn-user-callback (cffi:callback debug-callback) 
				   :user-data (cffi:null-pointer)))
	(all-extensions (get-all-instance-extensions))
	(all-layers (get-all-instance-layers))
	(sdl-extensions (sdl-vulkan:sdl-get-instance-extensions (get-window sys))))
    (when (we.dbg:vk-debug-p)
      (if (find "VK_EXT_debug_utils" all-extensions :test #'string=)
  	  (pushnew "VK_EXT_debug_utils" (vk:enabled-extension-names create-info))
  	  (error "not support vk_ext_debug_report"))
      (when (find "VK_LAYER_KHRONOS_validation" all-layers :test #'string=)
	(pushnew "VK_LAYER_KHRONOS_validation" (vk:enabled-layer-names create-info)))
      (setf (vk:next create-info) debug-report-create-info))
    (let* ((layers (vk:enabled-layer-names create-info))
	   (extensions (vk:enabled-extension-names create-info))
	   (uexts (remove-duplicates (append sdl-extensions extensions)
				     :test #'string=))
	   (ulays layers))
      (we.dbg:msg :app "~2tuse layer [~a]~%" ulays)
      (we.dbg:msg :app "~2tuse extensions [~a]~%" uexts)
      (setf (vk:enabled-extension-names create-info) uexts
	    (vk:enabled-layer-names create-info) ulays)
      (multiple-value-bind (cinstance cresult) (check-result #'vk:create-instance create-info)
	(declare (ignore cresult))
	(set-instance sys cinstance)
	(setf vk:*default-extension-loader* (vk:make-extension-loader :instance cinstance))
	(when (we.dbg:vk-debug-p)
	  (set-dbg-reporter sys (create-debug-report-callback cinstance debug-report-create-info))
	  (we.dbg:msg :app "~2tcreate debug-reporter ~a~%" (get-dbg-reporter sys)))
	(we.dbg:msg :app "~2tcreate instance ~a~%" cinstance)))))

(defun vk->destroy-instance (sys)
  (we.dbg:msg :app "destroy instance: -> ~%")
  (when (and (get-dbg-reporter sys) (get-instance sys))
    (we.dbg:msg :app "~2tdestroy dbg reporter [~a] ~%" (get-dbg-reporter sys))
    (vk:destroy-debug-utils-messenger-ext (get-instance sys) (get-dbg-reporter sys)))
  (when (get-instance sys)
    (we.dbg:msg :app "~2tdestroy instance [~a] ~%" (get-instance sys))
    (vk:destroy-instance (get-instance sys))
    (set-instance sys nil)))
