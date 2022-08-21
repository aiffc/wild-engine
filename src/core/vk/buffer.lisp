(in-package :%wild-engine.core.vk)

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

;; (defun build-cffi->lisp-body (bodies))

;; (defun build-lisp->cffi-body (bodies))

(defmacro defbuffer (name (&key (usage :uniform) (binding 0)) &body body)
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
	 (make-body (we.u:gen-setf struct-initarg struct-atoms)) ;; class make function
	 ;; vertex infos
	 (vbind-info-fun (we.u:create-symbol name '-vertex-binding-info))
	 (vattr-info-fun (we.u:create-symbol name '-vertex-attribute-info)))
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
	  `(progn
	     (defun ,vattr-info-fun (&optional (input-rate :vertex))
	       (vk:make-vertex-input-binding-description
		:binding ,binding
		:stride (cffi:foreign-type-size '(:struct ,name))
		:input-rate input-rate))
	     (defun ,vbind-info-fun ()
	       (list ,@(loop :for i :from 0 :below (length body)
			     :for bd := (nth i body)
			     :for sname := (first bd)
			     :for stype := (second bd)
			     :do (format t "~a~%" stype)
			     :collect `(vk:make-vertex-input-attribute-description
					:binding ,binding
					:location ,i
					:format ,(case stype
						   (:vec2 :r32g32-sfloat)
						   (:vec3 :r32g32b32-sfloat)
						   (:vec4 :r32g32b32a32-sfloat))
					:offset (cffi:foreign-slot-offset '(:struct ,name)
									  ',sname))))))))))
