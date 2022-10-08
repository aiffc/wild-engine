(in-package :pbox)

(defparameter *game-pipeline* nil)

(defparameter *map-width* 900)
(defparameter *map-height* 900)

(defun init-pipeline (sys)
  (setf *game-pipeline* (makeg-obj-pipeline sys *obj-layout*)))

(defun destroy-pipeline (sys)
  (destroyg-obj-pipeline sys *game-pipeline*)
  (setf *game-pipeline* nil))

(defun draw-map (sys)
  (with-gcmd (cmd sys 0 0 *map-width* *map-height* #(1.0 1.0 1.0 1.0)) ;; record command buffer
    (bind-gpipeline cmd *game-pipeline*)
    (set-viewport cmd :width (* 1.0 *map-width*) :height (* 1.0 *map-height*))
    (set-scissor cmd :width *map-width* :height *map-height*)
    (draw-player sys cmd)))

(defun init-game-obj (sys)
  (init-obj sys)
  (init-pipeline sys)
  (init-player sys))

(defun destroy-game-obj (sys)
  (destroy-player sys)
  (destroy-pipeline sys)
  (destroy-obj sys))

(we.dbg:dbg-trace :fps)

(defun pbox ()
  (with-we-init (sys :w *map-width* :h *map-height* :x 0 :y 0 :title "test") ;; we initialize
    ;; ready buffers
    (init-game-obj sys)
    (draw-map sys)
    (with-we-main-loop ()
      (:keydown (:keysym key)
		(key-event key sys)
		(draw-map sys))
      (:idle ()
	     (draw-map sys)
	     ;; (updatedu-obj-uniform sys *obj-uniform* 0 (build-transfer (+ (player-x *player*) 0.01) (player-y *player*)))
	     ;; (mapdu-obj-uniform sys *obj-uniform*)
	     ;; (draw-map sys)
	     ;; (updatedu-obj-uniform sys *obj-uniform* 0 (build-transfer (- (player-x *player*) 0.01) (player-y *player*)))
	     ;; (mapdu-obj-uniform sys *obj-uniform*)
	     ;; (draw-map sys)
	     ))
    (destroy-game-obj sys)))
