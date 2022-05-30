
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
   #:memcpy))

(defpackage :%wild-engine.debug
  (:use #:cl)
  (:nicknames #:%we.dbg)
  (:export
   #:msg
   #:vk-debug-p
   #:dbg-trace
   #:dbg-untrace))

(defpackage :%wild-engine.math
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

(defpackage :%wild-engine.core.utils
  (:use #:cl)
  (:nicknames #:%we.utils)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg))
  (:export
   #:*apps*
   #:window
   #:vk.instance
   #:instance
   #:instance-reporter
   #:vk.surface
   #:surface
   #:vk.gpu
   #:gpu
   #:gpu-capabilities
   #:gpu-formats
   #:gpu-present-mode
   #:gpu-features
   #:gpu-properties
   #:gpu-queue-families
   #:gpu-memory-infos
   #:vk.device
   #:device
   #:device-gqueue-family-index
   #:device-cqueue-family-index
   #:device-tqueue-family-index
   #:device-pqueue-family-index
   #:device-gqueues
   #:device-cqueues
   #:device-tqueues
   #:device-pqueues
   #:vk.swapchain
   #:swapchain
   #:swapchain-format
   #:swapchain-images
   #:swapchain-image-views
   #:vk.render-pass
   #:render-pass
   #:vk.framebuffer
   #:framebuffer
   #:vk.cmd-pool
   #:cmd-pool-graphics
   #:cmd-pool-compute
   #:vk.cmds
   #:cmds-graphics
   #:cmds-compute
   #:vk.signal
   #:signal-image-available
   #:signal-render-finish
   #:vk.layout
   #:layout
   #:layout-descriptor-pool
   #:layout-descriptor-set-layout
   #:layout-descriptor-sets
   #:layout-uniform-buffers
   #:app
   #:make-app
   #:destroy-app
   #:app-handle))

(defpackage :%wild-engine.core.windows
  (:use #:cl)
  (:nicknames #:%we.win)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg)
   (:%wild-engine.core.utils #:%we.utils)
   (:wild-engine.utils #:we.u)))

(defpackage :%wild-engine.core.vulkan
  (:use #:cl)
  (:nicknames #:%we.vk)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg)
   (:%wild-engine.core.utils #:%we.utils)
   (:%wild-engine.math #:we.math)
   (#:rtg-math.vector2 #:v2)
   (#:rtg-math.vector3 #:v3)
   (#:rtg-math.vector4 #:v4)
   (#:rtg-math.matrix2 #:m2)
   (#:rtg-math.matrix3 #:m3)
   (#:rtg-math.matrix4 #:m4))
  (:export
   #:with-gcmd
   #:create-shader-stage
   #:destroy-shader-stages
   #:graphics-pipeline-create-info
   #:layout-create-info
   #:create-graphics-pipeline
   #:get-gpipeline
   #:free-buffer
   #:create-vertex-buffer
   #:vertex->mem
   #:vertex
   #:create-index-buffer
   #:destroy-uniform-buffer))

(defpackage :%wild-engine.core.app
  (:use #:cl)
  (:nicknames #:%we.app)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg)
   (:%wild-engine.core.utils #:%we.utils)))

(defpackage :wild-engine.api
  (:use #:cl)
  (:nicknames #:we.api)
  (:local-nicknames
   (:%wild-engine.core.vulkan #:%we.vk)
   (:%wild-engine.core.utils #:%we.utils)
   (:wild-engine.utils #:we.u))
  (:export
   #:with-app
   #:defkey-down
   #:defkey-up
   #:defmouse
   #:with-main-loop
   #:with-render
   #:define-shader-stage
   #:define-graphics-pipeline
   #:define-vertex
   #:make-vertex
   #:define-index
   #:bind-graphics-pipeline
   #:define-layout
   #:set-viewport
   #:set-scissor
   #:set-vertex
   #:set-index
   #:draw))
