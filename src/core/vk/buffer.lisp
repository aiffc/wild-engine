(in-package :%wild-engine.core.vk)

(defstruct vkbuffer
  (buffer nil)
  (memory nil))

(defstruct vbuffer
  (buffer nil) ;; vkbuffer
  (size 0))

(defstruct ibuffer
  (buffer nil)
  (size 0))

(defun create-buffer (sys size usage properties
		      &aux
			(device (get-device sys)))
  "function for create buffer and allocate memory for the buffer"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((create-info (vk:make-buffer-create-info
		       :size size
		       :usage usage
		       :sharing-mode :exclusive))
	 (buffer (check-result #'vk:create-buffer device create-info))
	 (req (vk:get-buffer-memory-requirements device buffer))
	 (mem-size (vk:size req))
	 (type-index (find-memory sys
				  (vk:memory-type-bits req)
				  properties))
	 (alloc-info (vk:make-memory-allocate-info
		      :allocation-size mem-size
		      :memory-type-index type-index))
	 (memory (check-result #'vk:allocate-memory device alloc-info)))
    (we.dbg:msg :app "create buffer ~a~%" buffer)
    (we.dbg:msg :app "alloc memory ~a~%" memory)
    (vk:bind-buffer-memory device buffer memory 0)
    ;; stage buffer and memory free in self function
    (values (make-vkbuffer :buffer buffer :memory memory))))

(defun destroy-buffer (sys buffer
		       &aux
			 (device (get-device sys)))
  (we.dbg:msg :app "free memory ~a~%" (vkbuffer-memory buffer))
  (vk:free-memory device (vkbuffer-memory buffer))
  (we.dbg:msg :app "destroy buffer ~a~%" (vkbuffer-buffer buffer))
  (vk:destroy-buffer device (vkbuffer-buffer buffer)))

(defun copy-buffer (sys src dst size)
  "function used to copy buffer from src to dst"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((region (vk:make-buffer-copy
		 :src-offset 0
		 :dst-offset 0
		 :size size)))
    (with-transfer-cmd (sys cmd)
      (vk:cmd-copy-buffer cmd src dst (list region)))))

;; buffer defination
(defun %parser-body (bd
		     &aux
		       (name (first bd))
		       (type (second bd)))
  ;; function used to build cffi struct body
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (list name
	(case type
	  (:vec2 '(:struct we.math:vec2))
	  (:vec3 '(:struct we.math:vec3))
	  (:vec4 '(:struct we.math:vec4))
	  (:mat2 '(:struct we.math:mat2))
	  (:mat3 '(:struct we.math:mat3))
	  (:mat4 '(:struct we.math:mat4))
	  (t type))))

(defun %parser-dynamic-body (bd
			     &aux
			       (name (first bd))
			       (type (second bd)))
  ;; function used to build cffi struct body
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (list name
	(case type
	  (:vec2 '(:pointer (:struct we.math:vec2)))
	  (:vec3 '(:pointer (:struct we.math:vec3)))
	  (:vec4 '(:pointer (:struct we.math:vec4)))
	  (:mat2 '(:pointer (:struct we.math:mat2)))
	  (:mat3 '(:pointer (:struct we.math:mat3)))
	  (:mat4 '(:pointer (:struct we.math:mat4)))
	  (t `(:pointer ,type)))))

(defun get-atom-size (bd &aux
			   (type (second bd)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case type
    (:vec2 (cffi:foreign-type-size '(:struct we.math:vec2)))
    (:vec3 (cffi:foreign-type-size '(:struct we.math:vec3)))
    (:vec4 (cffi:foreign-type-size '(:struct we.math:vec4)))
    (:mat2 (cffi:foreign-type-size '(:struct we.math:mat2)))
    (:mat3 (cffi:foreign-type-size '(:struct we.math:mat3)))
    (:mat4 (cffi:foreign-type-size '(:struct we.math:mat4)))
    (t (cffi:foreign-type-size type))))

(defun build-cffi-struct-body (bodies dynamic-p)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop :for bd :in bodies
	:collect (if dynamic-p
		     (%parser-dynamic-body bd)
		     (%parser-body bd))))

(defun %gen-alloc (bd
		   &aux
		     (name (first bd))
		     (type (second bd))
		     (set-sym (gensym)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case type
    (:vec2 (list set-sym `(we.math:alloc-vec2 (,name value))))
    (:vec3 (list set-sym `(we.math:alloc-vec3 (,name value))))
    (:vec4 (list set-sym `(we.math:alloc-vec4 (,name value))))
    (:mat2 (list set-sym `(we.math:alloc-mat2 (,name value))))
    (:mat3 (list set-sym `(we.math:alloc-mat3 (,name value))))
    (:mat4 (list set-sym `(we.math:alloc-mat4 (,name value))))
    (t (list set-sym `(,name value)))))

(defun gen-alloc (body)
  "generate body to fill struct"
  (loop :for bd :in body
	:collect (%gen-alloc bd)))

(defun get-struct-atoms (bodies)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (mapcar #'first bodies))

(defun get-uniform-buffer (uniform)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (first uniform))

(defun get-uniform-memory (uniform)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (second uniform))

(defun get-uniform-buffer-by-index (uniforms index)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vkbuffer-buffer (nth index uniforms)))

(defun get-uniform-memory-by-index (uniforms index)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vkbuffer-memory (nth index uniforms)))

(defstruct dynamic-uniform
  (buffer nil)
  (ptr nil))


(defmacro defbuffer (name (&key (usage :uniform) (binding 0) (dynamic-p nil) (dynamic-count 1)) &body body)
  "
usage ->
  (defbuffer name (:usage :vertex :binding 0)
    (pos :vec2)
    (color :vec3))
export
  make-*name*
:usage unifrom export
  createu-*name*
  destroyu-*name*
  updateu-*name*
  createu-*name*
  withu-*name*
:usage vertex export
  *name*-vertex-binding-info
  *name*--vertex-attribute-info
  createv-*name*
  withv-*name*
:usage dynamic uniform export
  alitnmentdu-*name*
  createdu-*name*
  destroydu-*name*
  mapdu-*name*
  updatedu-*name*
"
  (let* ((struct-cname (we.u:create-symbol 'c- name))                 ;; struct c name
	 (struct-atoms (get-struct-atoms body))                       ;; struct member names
	 (struct-body (build-cffi-struct-body body dynamic-p))        ;; defcstruct body
	 (alloc-body (gen-alloc body))                                ;; translate into foreign alloc body
	 (alloc-symbols (mapcar #'first alloc-body))                       ;;  get alloc body symbols
	 (alloc-set-body (we.u:gen-setf struct-atoms alloc-symbols))  ;; translate into foreign set body
	 ;; lisp class function
	 (make-fun (we.u:create-symbol 'make- name))
	 (struct-initarg (mapcar #'(lambda (sym)
			       (alexandria:make-keyword (format nil "~a" sym)))
			    struct-atoms))
	 (make-body (we.u:gen-setf struct-initarg struct-atoms))) ;; class make function
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute))
       (cffi:defcstruct (,name :class ,struct-cname)
	 ,@struct-body)
       (defclass-std:defclass/std ,name ()
	 (,struct-atoms))
       ,(unless dynamic-p
	  ;; do not support for tranlate dynamic uniform buffer
	  `(progn
	     (defun ,make-fun (&key ,@struct-atoms)
	       (make-instance ',name ,@make-body))
	     (defmethod cffi:translate-from-foreign (ptr (type ,struct-cname))
	       (cffi:with-foreign-slots (,struct-atoms ptr (:struct ,name))
		 (,make-fun ,@ (we.u:gen-setf struct-initarg struct-atoms))))
	     (defmethod cffi:translate-into-foreign-memory (value (type ,struct-cname) ptr)
	       (cffi:with-foreign-slots (,struct-atoms ptr (:struct ,name))
		 (we.u:with-cffi-alloc (,@alloc-body)
		   (setf ,@alloc-set-body))))))
       ,(when (eql usage :vertex)
	  ;; vertex infos
	  (let ((vbind-info-fun (we.u:create-symbol name '-vertex-binding-info))
		(vattr-info-fun (we.u:create-symbol name '-vertex-attribute-info))
		(vdata-fun (we.u:create-symbol 'createv- name))
		;;(vdata-macro (we.u:create-symbol 'withv- name))
		)
	    `(progn
	       (defun ,vbind-info-fun (&optional (input-rate :vertex))
		 (list (vk:make-vertex-input-binding-description
			:binding ,binding
			:stride (cffi:foreign-type-size '(:struct ,name))
			:input-rate input-rate)))
	       (defun ,vattr-info-fun ()
		 (list ,@(loop :for i :from 0 :below (length body)
			       :for bd := (nth i body)
			       :for sname := (first bd)
			       :for stype := (second bd)
			       :collect `(vk:make-vertex-input-attribute-description
					  :binding ,binding
					  :location ,i
					  :format ,(case stype
						     (:vec2 :r32g32-sfloat)
						     (:vec3 :r32g32b32-sfloat)
						     (:vec4 :r32g32b32a32-sfloat))
					  :offset (cffi:foreign-slot-offset '(:struct ,name)
									    ',sname)))))
	       (defun ,vdata-fun (sys data
				  &aux
				    (data-size (length data))
				    (size (* data-size
					     (cffi:foreign-type-size '(:struct ,name)))))
		 (cffi:with-foreign-object (ptr '(:struct ,name) data-size)
		   (loop :for i :from 0 :below data-size
			 :do (setf (cffi:mem-aref ptr '(:struct ,name) i)
				   (svref data i)))
		   (multiple-value-bind (sbuffer)
		       (create-buffer sys size :transfer-src '(:host-visible :host-coherent))
		     (map-memory sys (vkbuffer-memory sbuffer) ptr size)
		     (multiple-value-bind (buffer)
			 (create-buffer sys size '(:transfer-dst :vertex-buffer) :device-local)
		       (copy-buffer sys (vkbuffer-buffer sbuffer) (vkbuffer-buffer buffer) size)
		       ;; destroy stage buffer here
		       (destroy-buffer sys sbuffer)
		       (make-vbuffer :buffer buffer :size size)))))
	       ;; (defmacro ,vdata-macro ((buffer sys data) &body wbody)
	       ;; 	 (let ((vdata-fun (we.u:create-symbol 'createv- ',name))
	       ;; 	       (memory (gensym)))
	       ;; 	   `(multiple-value-bind (,buffer ,memory ,size) (,vdata-fun ,sys ,data)
	       ;; 	      ,@wbody
	       ;; 	      (destroy-buffer ,sys ,buffer ,memory))))
	       )))
       ,(when (eql usage :uniform)
	  ;; todo
	  (if dynamic-p
	      (let ((dynaimic-alignment (we.u:create-symbol 'alitnmentdu- name))  ;; dynamic uniform buffer just support one atom
		    (atom-alignment (get-atom-size (first body)))
		    (atom-name (first (first body)))
		    (atom-type (second (%parser-body (first body))))
		    (dynamic-uniform-buffer-alloc-fun (we.u:create-symbol 'createdu- name))
		    (dynamic-uniform-buffer-free-fun (we.u:create-symbol 'destroydu- name))
		    (dynamic-map-fun (we.u:create-symbol 'mapdu- name))
		    (dyanmic-update-fun (we.u:create-symbol 'updatedu- name)))
		`(progn
		   (defun ,dynaimic-alignment (sys
					       &aux
						 (min-aligment (vk:min-uniform-buffer-offset-alignment
								(vk:limits (get-gpu-properties sys))))
						 (alignment ,atom-alignment))
		     (when (> min-aligment 0)
		       (setf alignment (logand (+ alignment (- min-aligment 1))
					       (lognot (- min-aligment 1)))))
		     alignment)
		   (defun ,dynamic-uniform-buffer-alloc-fun (sys)
		     (let ((ptr (cffi:foreign-alloc '(:struct ,name))))
		       (setf (cffi:foreign-slot-value ptr '(:struct ,name) ',atom-name)
			     (cffi::%foreign-alloc (* ,dynamic-count (,dynaimic-alignment sys))))
		       (multiple-value-bind (buffer)
			   (create-buffer sys (* ,dynamic-count (,dynaimic-alignment sys)) :uniform-buffer :host-visible)
			 (make-dynamic-uniform :buffer buffer :ptr ptr))))
		   (defun ,dynamic-map-fun (sys buffer)
		     (map-memory sys
				 (vkbuffer-memory (dynamic-uniform-buffer buffer))
				 (cffi:foreign-slot-value (dynamic-uniform-ptr buffer) '(:struct ,name) ',atom-name)
				 (* ,dynamic-count (,dynaimic-alignment sys))))
		   (defun ,dyanmic-update-fun (sys buffer index val)
		     (setf (cffi:mem-ref (cffi:inc-pointer (cffi:foreign-slot-value (dynamic-uniform-ptr buffer) '(:struct ,name) ',atom-name)
							   (* index (,dynaimic-alignment sys)))
					 ',atom-type)
			   val))
		   (defun ,dynamic-uniform-buffer-free-fun (sys buffer)
		     (cffi:foreign-free (cffi:foreign-slot-value (dynamic-uniform-ptr buffer) '(:struct ,name) ',atom-name))
		     (cffi:foreign-free (dynamic-uniform-ptr buffer))
		     (destroy-buffer sys (dynamic-uniform-buffer buffer)))))
	      (let ((uniform-create-fun (we.u:create-symbol 'createu- name))
		    (uniform-destroy-fun (we.u:create-symbol 'destryu- name))
		    (uniform-buffer-update-fun (we.u:create-symbol 'updateu- name))
		    ;;(with-uniform (we.u:create-symbol 'withu- name))
		    )
		`(progn
		   (defun ,uniform-create-fun (sys count)
		     (loop :for i :from 0 :below count
			   :collect (create-buffer sys (cffi:foreign-type-size '(:struct ,name)) :uniform-buffer '(:host-visible :host-coherent))))
		   (defun ,uniform-destroy-fun (sys uniforms)
		     (mapcar (lambda (uniform)
			  (destroy-buffer sys uniform))
			uniforms))
		   (defun ,uniform-buffer-update-fun (sys uniforms index val)
		     (let ((memory (vkbuffer-memory (nth index uniforms))))
		       (cffi:with-foreign-object (obj '(:struct ,name))
			 (setf (cffi:mem-ref obj '(:struct ,name)) val)
			 (map-memory sys memory obj (cffi:foreign-type-size '(:struct ,name))))))
		   ;; (defmacro ,with-uniform ((uniforms sys count) &body wbody)
		   ;;   (let ((uniform-create-fun (we.u:create-symbol 'createu- ',name))
		   ;; 	   (uniform-destroy-fun (we.u:create-symbol 'destryu- ',name)))
		   ;;     `(let ((,uniforms (,uniform-create-fun ,sys ,count)))
		   ;; 	  (progn ,@wbody)
		   ;; 	  (,uniform-destroy-fun ,sys ,uniforms))))
		   )))))))

(defun create-index-buffer (sys data &optional (data-type :uint32)
			    &aux
			      (data-size (length data))
			      (size (* data-size
				       (cffi:foreign-type-size data-type))))
  (cffi:with-foreign-object (ptr data-type data-size)
    (loop :for i :from 0 :below data-size
	  :do (setf (cffi:mem-aref ptr data-type i)
		    (svref data i)))
    (multiple-value-bind (sbuffer)
	(create-buffer sys size :transfer-src '(:host-visible :host-coherent))
      (map-memory sys (vkbuffer-memory sbuffer) ptr size)
      (multiple-value-bind (buffer)
	  (create-buffer sys size '(:transfer-dst :index-buffer) :device-local)
	(copy-buffer sys (vkbuffer-buffer sbuffer) (vkbuffer-buffer buffer) size)
	;; destroy stage buffer here
	(destroy-buffer sys sbuffer)
	(make-ibuffer :buffer buffer :size data-size)))))

(defun destroy-index-buffer (sys index-buffer)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (destroy-buffer sys (ibuffer-buffer index-buffer)))

(defmacro with-index-buffer ((index-buffer sys data &optional (data-type :uint32)) &body body)
  "usage to create and destroy index buffer"
  `(let ((,index-buffer (create-index-buffer ,sys ,data ,data-type)))
     ,@body
     (destroy-index-buffer ,sys ,index-buffer)))
