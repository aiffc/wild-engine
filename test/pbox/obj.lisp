(in-package :pbox)

(defbuffer obj-vertex (:usage :vertex)
  (pos :vec2)
  (text-coord :vec2))

(defbuffer obj-uniform (:usage :uniform :dynamic-p t :dynamic-count 1)
  (model :mat4))

(defparameter *obj-vertex-pos*
  (vector (make-obj-vertex :pos #(-0.1 -0.1) ;; make test-vertex data
			   :text-coord #(1.0 0.0))
	  (make-obj-vertex :pos #(0.1 -0.1)
			   :text-coord #(0.0 0.0))
	  (make-obj-vertex :pos #(0.1 0.1)
			   :text-coord #(0.0 1.0))
	  (make-obj-vertex :pos #(-0.1 0.1)
			   :text-coord #(1.0 1.0))))

(defparameter *obj-index-data* (vector 0 1 2 2 3 0))

(defpipeline-layout obj-layout ()
  (:type :combined-image-sampler
   :count 1
   :flags :fragment)
  (:type :uniform-buffer-dynamic
   :count 1
   :flags :vertex))

(defgpipeline obj-pipeline ()
  (:vertex *vert*)
  (:fragment *frag*)
  (:vertex-stage obj-vertex))

(defdescriptor obj-descriptor (1) 
  (:type :combined-image-sampler
   :count 1)
  (:type :uniform-buffer-dynamic
   :count 1))

(defdescriptor-sets obj-set 1)

(defparameter *obj-layout* nil)
(defparameter *obj-descriptor-layout* nil)
(defparameter *obj-descriptor-pool* nil)
(defparameter *obj-sets* nil)
(defparameter *obj-uniform* nil)

(defun init-obj (sys)
  (setf *obj-descriptor-layout* (makedsl-obj-layout sys)
	*obj-layout* (makepl-obj-layout sys)
	*obj-descriptor-pool* (makedp-obj-descriptor sys)
	*obj-sets* (allocds-obj-set sys *obj-descriptor-pool* (list *obj-descriptor-layout*))
	*obj-uniform* (createdu-obj-uniform sys))
  (updateds-dynamic-buffer-obj-set sys *obj-sets* *obj-uniform*
				   :range (alitnmentdu-obj-uniform sys)
				   :binding 1))

(defun destroy-obj (sys)
  (destroydu-obj-uniform sys *obj-uniform*)
  (freeds-obj-set sys *obj-descriptor-pool* *obj-sets*)
  (destroypl-obj-layout sys *obj-layout* (list *obj-descriptor-layout*))
  (destroydp-obj-descriptor sys *obj-descriptor-pool*)
  (setf *obj-layout* nil
	*obj-descriptor-layout* nil
	*obj-sets* nil
	*obj-descriptor-layout* nil
	*obj-uniform* nil))

(defclass obj ()
  ((texture :initarg :texture :initform nil :accessor obj-texture)
   (ubuffer :initarg :ubuffer :initform nil :accessor obj-ubuffer)))
