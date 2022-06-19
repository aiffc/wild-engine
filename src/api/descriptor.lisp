(in-package :wild-engine.api)

(defun generate-uniform-descriptor-info (app args)
  "get uniform descriptor pool size info"
  (loop :repeat (length args)
	:collect (%we.vk:create-descriptor-size-info app :uniform-buffer)))

(defun generate-texture-descriptor-info (app args)
  "get uniform descriptor pool size info"
  (loop :repeat (length args)
	:collect (%we.vk:create-descriptor-size-info app :combined-image-sampler)))
