(defpackage :wild-engine
  (:use #:cl)
  (:nicknames #:we)
  (:export))

(defpackage :wild-engine.utils
  (:use #:cl)
  (:nicknames #:we.u)
  (:export
   #:create-symbol
   #:set-value
   #:to-single-float
   #:with-cffi-alloc
   #:with-mvalues
   #:gen-cons
   #:gen-setf
   #:memcpy))

(defpackage :wild-engine.debug
  (:use #:cl)
  (:nicknames #:we.dbg)
  (:export
   #:msg
   #:vk-debug-p
   #:dbg-trace
   #:dbg-untrace))

(defpackage :wild-engine.math
  (:use #:cl)
  (:nicknames #:we.math)
  (:local-nicknames
   (:wild-engine.utils #:we.u)
   (:rtg-math.vector2 #:v2)
   (:rtg-math.vector3 #:v3)
   (:rtg-math.vector4 #:v4)
   (:rtg-math.matrix2 #:m2)
   (:rtg-math.matrix3 #:m3)
   (:rtg-math.matrix4 #:m4))
  (:export
   #:vec2
   #:vec3
   #:vec4
   #:alloc-vec2
   #:alloc-vec2-xy
   #:alloc-vec3
   #:alloc-vec3-xyz
   #:alloc-vec4
   #:alloc-vec4-xyzw
   #:mat2
   #:mat3
   #:mat4
   #:alloc-mat2
   #:alloc-mat3
   #:alloc-mat4
   #:v2->v3
   #:v3->v4
   #:make-v2
   #:make-v3
   #:make-v4
   #:make-m2
   #:make-m3
   #:make-m4
   #:v*adjoint
   #:m3->m4))

(defpackage :%wild-engine.core.vk
  (:use #:cl)
  (:nicknames #:we.vk)
  (:local-nicknames
   (:wild-engine.utils #:we.u)
   (:wild-engine.debug #:we.dbg))
  (:export
   #:vk->init-all
   #:vk->destroy-all
   #:vbuffer-buffer
   #:vbuffer-size
   #:ibuffer-buffer
   #:ibuffer-size
   #:with-gcmd
   #:defbuffer
   #:defpipeline-layout
   #:defgpipeline
   #:withg-pipelines
   #:with-index-buffer
   #:destroy-buffer
   #:create-index-buffer
   #:destroy-index-buffer
   #:destroy-vkbuffer
   #:icount
   #:ibuffer
   #:defdescriptor-sets
   #:defdescriptor
   #:deftexture
   #:destroy-image))

(defpackage :we.ctrl
  (:use #:cl)
  (:nicknames #:we.ctrl)
  (:local-nicknames
   (:wild-engine.utils #:we.u)
   (:wild-engine.debug #:we.dbg))
  (:export
   #:with-we-init
   #:defkey-down
   #:defkey-up
   #:defmouse
   #:with-we-main-loop
   #:bind-gpipeline
   #:bind-descriptor-sets
   #:set-viewport
   #:set-scissor
   #:set-vertex
   #:set-index
   #:draw))
