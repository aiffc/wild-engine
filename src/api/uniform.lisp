(in-package :wild-engine.api)

(defparameter *uniform-hash* (make-hash-table))

(defun convert-type (stype)
  "convert math type to struct"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case stype
    (:vec2 '(:struct we.math:vec2))
    (:vec3 '(:struct we.math:vec3))
    (:vec4 '(:struct we.math:vec4))
    (:mat2 '(:struct we.math:mat2))
    (:mat3 '(:struct we.math:mat3))
    (:mat4 '(:struct we.math:mat4))
    (t stype)))

(defun parser-struct (body)
  "generate struct body"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop :for bd :in body
	:for name := (getf bd :accessor)
	:for type := (getf bd :type)
	:collect (list name (convert-type type))))

(defun class-convert-type (stype)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case stype
    (:vec2 'we.math:vec2)
    (:vec3 'we.math:vec3)
    (:vec4 'we.math:vec4)
    (:mat2 'we.math:mat2)
    (:mat3 'we.math:mat3)
    (:mat4 'we.math:mat4)
    (t stype)))

(defun gen-uniform-class (body)
  "generate class body"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop :for bd :in body
	:for name := (getf bd :accessor)
	:for ctype := (class-convert-type (getf bd :type))
	:for initarg := (getf bd :initarg)
	:for initform := (getf bd :initform)
	:collect (list name
		       :accessor name
		       :initarg initarg
		       :type ctype
		       :initform initform)))

(defun gen-alloc-uniform (lst)
  "generate body to single atom in struct"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((symbol-type (getf lst :type))
	(symbol-name (getf lst :accessor))
	(set-symbol (gensym)))
    (case symbol-type
      (:vec2 (list set-symbol `(we.math:alloc-vec2 (,symbol-name value))))
      (:vec3 (list set-symbol `(we.math:alloc-vec3 (,symbol-name value))))
      (:vec4 (list set-symbol `(we.math:alloc-vec4 (,symbol-name value))))
      (:mat2 (list set-symbol `(we.math:alloc-mat2 (,symbol-name value))))
      (:mat3 (list set-symbol `(we.math:alloc-mat3 (,symbol-name value))))
      (:mat4 (list set-symbol `(we.math:alloc-mat4 (,symbol-name value))))
      (t (list set-symbol `(,symbol-name value))))))

(defun generate-uniform-to-c-body (body)
  "generate body to fill struct"
  (loop :for bd :in body
	:collect (gen-alloc-uniform bd)))

(defun gen-set-body (alloc-body body)
  "generate body to set struct atom"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((asymbol (mapcar (lambda (bd) (getf bd :accessor)) body))
	(rsymbol (mapcar #'first alloc-body)))
    (loop :for i :from 0 :below (length asymbol)
	  :collect (list 'setf (nth i asymbol) (nth i rsymbol)))))

(defun parse-uniform-body (layout-name body
			   &aux (uargs (rest body)))
  "function used to generate a struct and translate method"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((sbody (getf uargs :struct))            ;; get struct body
	 (name (getf uargs :name))               ;; get struct name
	 (struct-symbols (mapcar (lambda (bd) (getf bd :accessor)) sbody))
	 (uname (we.u:create-symbol 'c- name))
	 (alloc-body (generate-uniform-to-c-body sbody))  ;; generate alloc body 
	 (set-body (gen-set-body alloc-body sbody))       ;; generate set body
	 (ucreate-fun (we.u:create-symbol 'create-uniform- name)))
    (push ucreate-fun (gethash layout-name *uniform-hash*))   ;; store uniform initialize functions
    `(progn
       (defclass ,name ()
	 (,@ (gen-uniform-class sbody)))
       (cffi:defcstruct (,name :class ,uname)
	 ,@ (parser-struct sbody))
       (defmethod cffi:translate-into-foreign-memory (value (type ,uname) ptr)
	 (cffi:with-foreign-slots (,struct-symbols ptr (:struct ,name))
	   (we.u:with-cffi-alloc (,@alloc-body)
	     ,@set-body)))
       (defmethod cffi:translate-from-foreign (ptr (type ,uname))
	 (cffi:with-foreign-slots (,struct-symbols ptr (:struct ,name))
	   (list ,@struct-symbols)))
       (defun ,ucreate-fun (app)
	 (%we.vk:create-uniform-buffer app (cffi:foreign-type-size '(:struct ,name)))))))

;; ((:uniform-buffer
;;       :name aa 
;;       :binding 0
;;       :struct ((a :vec2)       ;; uniform buffer info create uniform buffer here
;; 			 (b :vec2)))
;;  (:uniform-buffer
;;       :name aa 
;;       :binding 0
;;       :struct ((a :vec2)       ;; uniform buffer info create uniform buffer here
;; 			 (b :vec2))))

(defun parse-uniform-bodies (app args)
  "convert args to a structs"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop :for arg :in args
	:collect (parse-uniform-body app arg)))

(defun build-uniform-info (arg
		   &aux (uarg (rest arg)))
  "generate a descriptor set create info for uniform buffer"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-descriptor-set-layout-binding
   :binding (getf uarg :binding)
   :descriptor-type :uniform-buffer
   :descriptor-count (we.u:set-value uarg :count 1)
   :stage-flags :vertex
   :immutable-samplers nil))          ;; not support yet

(defun generate-uniform-info (args)
  "generate uniform buffer infos for descriptor layouts"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop :for arg :in args
	:collect (build-uniform-info arg)))
