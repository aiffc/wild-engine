(in-package :pbox)

(defclass player (obj)
  ((player-x :initarg :p-x :initform nil :accessor player-x)
   (player-y :initarg :p-y :initform nil :accessor player-y)
   (index-buffer :initarg :p-index :initform nil :accessor player-index)
   (vertex-buffer :initarg :p-vertex :initform nil :accessor player-vertex)
   (uniform-buffer :initarg :p-uniform :initform nil :accessor player-uniform)))

(defparameter *player-vertex-pos*
  (vector (make-obj-vertex :pos #(-1.0 -1.0) ;; make test-vertex data
			   :text-coord #(1.0 0.0))
	  (make-obj-vertex :pos #(1.0 -1.0)
			   :text-coord #(0.0 0.0))
	  (make-obj-vertex :pos #(1.0 1.0)
			   :text-coord #(0.0 1.0))
	  (make-obj-vertex :pos #(-1.0 1.0)
			   :text-coord #(1.0 1.0))))

(defbuffer move-act (:usage :uniform)
  (mov :mat3))

(defdescriptor-sets player-set 1)

(deftexture player-t *player-texture* :rgba)

(defparameter *player* nil)

(defun init-player (sys)
  (setf *player* (make-instance 'player
				:p-x 0.0
				:p-y 0.0
				:texture (maket-player-t sys)
				:p-vertex (createv-obj-vertex sys *player-vertex-pos*)
				:p-index (create-index-buffer sys *obj-index-data*)))
  (updatedu-obj-uniform sys *obj-uniform* 0 (build-transfer (player-x *player*) (player-y *player*)))
  (mapdu-obj-uniform sys *obj-uniform*))

(defun destroy-player (sys)
  (destroy-image sys (obj-texture *player*))
  (destroy-index-buffer sys (player-index *player*) :buffer)
  (setf *player* nil))

(defun draw-player (sys cmd)
  (set-vertex cmd (player-vertex *player*))
  (set-index cmd (player-index *player*))
  (updateds-image-player-set sys *obj-sets* (obj-texture *player*))
  (bind-descriptor-sets cmd *obj-layout* *obj-sets* (list 0))
  (draw cmd :buffer (player-index *player*) :index-p t))
