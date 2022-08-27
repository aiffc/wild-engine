(in-package :%wild-engine.core.vk)

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
    (values buffer memory)))

(defun destroy-buffer (sys buffer memory
		       &aux
			 (device (get-device sys)))
  (we.dbg:msg :app "free memory ~a~%" memory)
  (vk:free-memory device memory)
  (we.dbg:msg :app "destroy buffer ~a~%" buffer)
  (vk:destroy-buffer device buffer))

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

(defun build-cffi-struct-body (bodies)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop :for bd :in bodies
	:collect (%parser-body bd)))

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

(defmacro defbuffer (name (&key (usage :uniform) (binding 0)) &body body)
  "
usage ->
  (defbuffer test-vertex (:usage :vertex :binding 0)
    (pos :vec2)
    (color :vec3))
"
  (let* ((struct-cname (we.u:create-symbol 'c- name))   ;; struct c name
	 (struct-atoms (get-struct-atoms body))         ;; struct member names
	 (struct-body (build-cffi-struct-body body))    ;; defcstruct body
	 (alloc-body (gen-alloc body))                  ;; translate into foreign alloc body
	 (alloc-symbols (mapcar #'first alloc-body))         ;;  get alloc body symbols
	 (alloc-set-body (we.u:gen-setf struct-atoms alloc-symbols)) ;; translate into foreign set body
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
       (defun ,make-fun (&key ,@struct-atoms)
	 (make-instance ',name ,@make-body))
       (defmethod cffi:translate-from-foreign (ptr (type ,struct-cname))
       	 (cffi:with-foreign-slots (,struct-atoms ptr (:struct ,name))
	   (,make-fun ,@ (we.u:gen-setf struct-initarg struct-atoms))))
       (defmethod cffi:translate-into-foreign-memory (value (type ,struct-cname) ptr)
       	 (cffi:with-foreign-slots (,struct-atoms ptr (:struct ,name))
	   (we.u:with-cffi-alloc (,@alloc-body)
	     (setf ,@alloc-set-body))))
       ,(when (eql usage :vertex)
	  ;; vertex infos
	  (let ((vbind-info-fun (we.u:create-symbol name '-vertex-binding-info))
		(vattr-info-fun (we.u:create-symbol name '-vertex-attribute-info))
		(vdata-fun (we.u:create-symbol 'createv- name))
		(vdata-macro (we.u:create-symbol 'withv- name)))
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
		   (multiple-value-bind (sbuffer smemory)
		       (we.vk::create-buffer sys size :transfer-src '(:host-visible :host-coherent))
		     (we.vk::map-memory sys smemory ptr size)
		     (multiple-value-bind (buffer memory)
			 (we.vk::create-buffer sys size '(:transfer-dst :vertex-buffer) :device-local)
		       (we.vk::copy-buffer sys sbuffer buffer size)
		       ;; destroy stage buffer here
		       (we.vk::destroy-buffer sys sbuffer smemory)
		       (values buffer memory size)))))
	       (defmacro ,vdata-macro ((sys data buffer memory size) &body wbody)
		 (let ((vdata-fun (we.u:create-symbol 'createv- ',name)))
		   `(multiple-value-bind (,buffer ,memory ,size) (,vdata-fun ,sys ,data)
		      ,@wbody
		      (we.vk::destroy-buffer ,sys ,buffer ,memory))))))))))

;; -----------------------------------test--------------------------------------
(defclass vertex ()
  ((v
    :accessor vertex-v
    :initarg :vertex-v
    :type simple-array
    :initform (v3:make 0.0 0.0 0.0))
   (vt
    :accessor vertex-vt
    :initarg :vertex-vt
    :type simple-array
    :initform (v3:make 0.0 0.0 0.0))
   (normal
    :accessor vertex-vn
    :initarg :vertex-vn
    :type simple-array
    :initform (v3:make 0.0 0.0 0.0))))

;; (defmethod print-unreadable-object ((obj vertex) stream)
;;   (print-unreadable-object (obj stream :type t :identity t)
;;     (format t "v:  ~a~%" v)
;;     (format t "vn: ~a~%" vn)
;;     (format t "vt: ~a~%" vt)))

(cffi:defcstruct (vertex :class c-vertex)
  (v (:struct we.math:vec3))
  (vt (:struct we.math:vec3))
  (vn (:struct we.math:vec3)))

(defun make-vertex (&key
		      (v (v3:make 0.0 0.0 0.0))
		      (vt (v3:make 0.0 0.0 0.0))
		      (vn (v3:make 0.0 0.0 0.0)))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-instance 'vertex :vertex-v v
			 :vertex-vt vt
			 :vertex-vn vn))

(defmethod cffi:translate-into-foreign-memory (value (handle c-vertex) ptr)
  (cffi:with-foreign-slots ((v vt vn) ptr (:struct vertex))
    (we.u:with-cffi-alloc ((cv (we.math:alloc-vec3 (vertex-v value)))
			   (cvt (we.math:alloc-vec3 (vertex-vt value)))
			   (cvn (we.math:alloc-vec3 (vertex-vn value))))
      (setf v cv vt cvt vn cvn))))

(defmethod cffi:translate-from-foreign (ptr (handle c-vertex))
  (cffi:with-foreign-slots ((v vt vn) ptr (:struct vertex)) 
    (make-vertex :v v :vt vt :vn vn)))

(defun vertex->mem (val size)
  (cffi:with-foreign-object (ptr '(:struct vertex) size)
    (loop :for i from 0 :below size
	  :do (progn
		(setf (cffi:mem-aref ptr '(:struct vertex) i)
		      (aref val i))))
    ptr))

(defun vertex-size ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (cffi:foreign-type-size '(:struct vertex)))

(defun vertex-input-stage ()
  "set the vertex input state can set it by define-vertex-stage"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (vk:make-pipeline-vertex-input-state-create-info
   :vertex-binding-descriptions
   (list
    (vk:make-vertex-input-binding-description
     :input-rate :vertex
     :binding 0
     :stride (vertex-size)))
   :vertex-attribute-descriptions
   (list
    (vk:make-vertex-input-attribute-description
     :location 0
     :binding 0
     :format :r32g32b32-sfloat
     :offset (cffi:foreign-slot-offset '(:struct vertex) 'v))
    (vk:make-vertex-input-attribute-description
     :location 1
     :binding 0
     :format :r32g32b32-sfloat
     :offset (cffi:foreign-slot-offset '(:struct vertex) 'vt))
    (vk:make-vertex-input-attribute-description
     :location 2
     :binding 0
     :format :r32g32b32-sfloat
     :offset (cffi:foreign-slot-offset '(:struct vertex) 'vn)))))

(defun create-vertex-buffer (sys data
			     &aux
			       (data-size (length data))
			       (size (* data-size
					(cffi:foreign-type-size '(:struct vertex)))))
  (cffi:with-foreign-object (ptr '(:struct vertex) data-size)
    (loop :for i :from 0 :below data-size
	  :do (setf (cffi:mem-aref ptr '(:struct vertex) i)
		    (svref data i)))
    (multiple-value-bind (sbuffer smemory)
	(we.vk::create-buffer sys size :transfer-src '(:host-visible :host-coherent))
      (we.vk::map-memory sys smemory ptr size)
      (multiple-value-bind (buffer memory)
	  (we.vk::create-buffer sys size '(:transfer-dst :vertex-buffer) :device-local)
	(we.vk::copy-buffer sys sbuffer buffer size)
	;; destroy stage buffer here
	(we.vk::destroy-buffer sys sbuffer smemory)
	(values buffer memory size)))))

(defparameter *test-data*
  (vector (make-vertex :v #(0.0 -0.5 0.0) :vt #(1.0 1.0 0.0))
	  (make-vertex :v #(0.5 0.5 0.0) :vt #(0.0 1.0 1.0))
	  (make-vertex :v #(-0.5 0.5 0.0) :vt #(1.0 0.0 1.0))))

(defmacro with-test-vertex-buffer ((sys buffer memory size) &body body)
  `(multiple-value-bind (,buffer ,memory ,size)
       (create-vertex-buffer ,sys *test-data*)
     ,@body
     (destroy-buffer ,sys ,buffer ,memory)))
