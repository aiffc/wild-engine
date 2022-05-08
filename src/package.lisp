(defpackage :wild-engine
  (:use #:cl)
  (:nicknames #:we)
  (:export))

(defpackage :wild-engine.utils
  (:use #:cl)
  (:nicknames #:we.u)
  (:export))

(defpackage :%wild-engine.debug
  (:use #:cl)
  (:nicknames #:%we.dbg)
  (:export
   #:msg
   #:vk-debug-p
   #:dbg-trace
   #:dbg-untrace))

(defpackage :%wile-engine.core.utils
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
   #:app
   #:make-app
   #:destroy-app
   #:app-handle
   #:set-value))

(defpackage :%wile-engine.core.windows
  (:use #:cl)
  (:nicknames #:%we.win)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg)
   (:%wile-engine.core.utils #:%we.utils)))

(defpackage :%wile-engine.core.vulkan
  (:use #:cl)
  (:nicknames #:%we.vk)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg)
   (:%wile-engine.core.utils #:%we.utils))
  (:export
   #:with-gcmd))

(defpackage :%wile-engine.core.app
  (:use #:cl)
  (:nicknames #:%we.app)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg)
   (:%wile-engine.core.utils #:%we.utils)))

(defpackage :wild-engine.api
  (:use #:cl)
  (:nicknames #:we.api)
  (:local-nicknames
   (:%wile-engine.core.vulkan #:%we.vk))
  (:export
   #:with-app
   #:defkey-down
   #:defkey-up
   #:defmouse
   #:with-main-loop
   #:with-render))
