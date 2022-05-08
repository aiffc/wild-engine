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
   #:vk.gpu
   #:vk.surface
   #:vk.device
   #:vk.swapchain
   #:vk.render-pass
   #:vk.framebuffer
   #:vk.cmd-pool
   #:vk.cmds
   #:vk.signal
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
   (:%wile-engine.core.utils #:%we.utils)))

(defpackage :%wile-engine.core.app
  (:use #:cl)
  (:nicknames #:%we.app)
  (:local-nicknames
   (:%wild-engine.debug #:%we.dbg)
   (:%wile-engine.core.utils #:%we.utils)))

(defpackage :wild-engine.api
  (:use #:cl)
  (:nicknames #:we.api)
  (:export
   #:with-app
   #:defkey-down
   #:defkey-up
   #:defmouse
   #:with-main-loop))
