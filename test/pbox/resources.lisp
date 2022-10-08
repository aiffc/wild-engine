(in-package :pbox)

(defparameter *pbox-path* (namestring (asdf:system-relative-pathname :wild-engine "test/pbox/")))
(defparameter *vert* (concatenate 'string *pbox-path* "shader/vert.spv"))
(defparameter *frag* (concatenate 'string *pbox-path* "shader/frag.spv"))
(defparameter *player-texture* (concatenate 'string *pbox-path* "texture/player.png"))

