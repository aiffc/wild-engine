(in-package :%wild-engine.core.vulkan)

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

(define-buffer-fun create-vertex-buffer (:vertex-buffer (vertex-size))
    "function for create vertex buffer")
