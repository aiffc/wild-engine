(in-package :%wild-engine.core.vulkan)

(define-buffer-fun create-index-buffer (:index-buffer (cffi:foreign-type-size :uint32))
    "function for create index buffer")
