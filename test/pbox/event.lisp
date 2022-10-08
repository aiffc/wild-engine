(in-package :pbox)

(defun build-transfer (x y)
  (m4:make 0.1 0.0 0.0 x
	   0.0 0.1 0.0 y
	   0.0 0.0 1.0 0.0
	   0.0 0.0 0.0 1.0))

(defun key-event (key sys)
  (let ((keyval (sdl2:scancode-value key)))
    (cond ((sdl2:scancode= keyval :scancode-w)
	   (when (> (player-y *player*) -0.9)
	     (setf (player-y *player*) (- (player-y *player*) 0.1))))
	  ((sdl2:scancode= keyval :scancode-s)
	   (when (< (player-y *player*) 0.9)
	     (setf (player-y *player*) (+ (player-y *player*) 0.1))))
	  ((sdl2:scancode= keyval :scancode-d)
	   (when (< (player-x *player*) 0.9)
	     (setf (player-x *player*) (+ (player-x *player*) 0.1))))
	  ((sdl2:scancode= keyval :scancode-a)
	   (when (> (player-x *player*) -0.9)
	     (setf (player-x *player*) (- (player-x *player*) 0.1)))))
    (updatedu-obj-uniform sys *obj-uniform* 0 (build-transfer (player-x *player*) (player-y *player*)))
    (mapdu-obj-uniform sys *obj-uniform*)))
