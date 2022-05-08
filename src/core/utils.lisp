(in-package #:%wile-engine.core.utils)

(defparameter *apps* (make-hash-table))

(defclass app-base () ())

(defmethod make-app :before (app (handle app-base) args)
  (declare (ignore handle args))
  (%we.dbg:msg :app "start initialize app [~a]~%" app))

(defmethod destroy-app :around (app (handle app-base))
  (declare (ignore handle))
  (%we.dbg:msg :app "start destroy app ~a~%" app)
  (when (next-method-p)
    (call-next-method)))

;; for sdl window
(defclass window (app-base)
  ((window
    :initarg :window
    :initform nil
    :accessor window))
  (:documentation
   "class used to store window handle"))

;; for vulkan handle
(defclass vk.instance (window)
  ((instance
    :initarg :instance
    :initform nil
    :accessor instance)
   (instance-reporter
    :initarg :instance-reporter
    :initform nil
    :accessor instance-reporter))
  (:documentation
   "store the instance and debug reporter handle for vulkan"))

(defclass vk.surface (vk.instance)
  ((surface
    :initarg :surface
    :initform nil
    :accessor surface))
  (:documentation
   "used to store surface handle"))

(defclass vk.gpu (vk.surface)
  ((gpu
    :initarg :gpu
    :initform nil
    :accessor gpu)
   (gpu-capabilities
    :initarg :capabilities
    :initform nil
    :accessor gpu-capabilities)
   (gpu-formats
    :initarg :formats
    :initform nil
    :accessor gpu-formats)
   (gpu-present-mode
    :initarg :present-mode
    :initform nil
    :accessor gpu-present-mode)
   (gpu-features
    :initarg :features
    :initform nil
    :accessor gpu-features)
   (gpu-properties
    :initarg :properties
    :initform nil
    :accessor gpu-properties)
   (gpu-queue-families
    :initarg :queue-families
    :initform nil
    :accessor gpu-queue-families)
   (gpu-memory-infos
    :initarg :memory-infos
    :initform nil
    :accessor gpu-memory-infos))
  (:documentation
   "used to store gpu info"))

(defclass vk.device (vk.gpu)
  ((device
    :initarg :device
    :initform nil
    :accessor device)
   (device-gqueue-family-index                    ;; selected queue family index for graphics
    :initarg :gqueuef-index
    :initform nil
    :accessor device-gqueue-family-index)
   (device-cqueue-family-index                    ;; selected queue family index for compute
    :initarg :cqueuef-index
    :initform nil
    :accessor device-cqueue-family-index)
   (device-tqueue-family-index                    ;; selected queue family index for transfer
    :initarg :tqueuef-index
    :initform nil
    :accessor device-tqueue-family-index)
   (device-pqueue-family-index                    ;; selected queue family index for present
    :initarg :pqueuef-index
    :initform nil
    :accessor device-pqueue-family-index)
   (device-gqueues 
    :initarg :gqueues
    :initform nil
    :accessor device-gqueues)
   (device-cqueues 
    :initarg :cqueues
    :initform nil
    :accessor device-cqueues)
   (device-tqueues 
    :initarg :tqueues
    :initform nil
    :accessor device-tqueues)
   (device-pqueues 
    :initarg :pqueues
    :initform nil
    :accessor device-pqueues))
  (:documentation
   "logic device handle for vulkan device"))

(defclass vk.swapchain (vk.device)
  ((swapchain
    :initarg :swapchain
    :initform nil
    :accessor swapchain)
   (format
    :initarg :format
    :initform nil
    :accessor swapchain-format)
   (images
    :initarg :images
    :initform nil
    :accessor swapchain-images)
   (image-views
    :initarg :image-views
    :initform nil
    :accessor swapchain-image-views))
  (:documentation
   "used to store swapchain handle"))

(defclass vk.render-pass (vk.swapchain)
  ((render-pass
    :initarg :render-pass
    :initform nil
    :accessor render-pass))
  (:documentation
   "used to store render pass handle"))

(defclass vk.framebuffer (vk.render-pass)
  ((framebuffer
    :initarg :framebuffer
    :initform nil
    :accessor framebuffer))
  (:documentation
   "used to store framebuffer handle"))

(defclass vk.cmd-pool (vk.framebuffer)
  ((cmd-pool-graphics
    :initarg :cpg
    :initform nil
    :accessor cmd-pool-graphics)
   (cmd-pool-compute
    :initarg :cpc
    :initform nil
    :accessor cmd-pool-compute))
  (:documentation
   "used to store graphics command pool and compute command pool"))

(defclass vk.cmds (vk.cmd-pool)
  ((cmds-graphics
    :initarg :cbg
    :initform nil
    :accessor cmds-graphics)
   (cmds-compute
    :initarg :cbc
    :initform nil
    :accessor cmds-compute))
  (:documentation
   "used to store graphics command buffers and compute command buffers"))

(defclass vk.signal (vk.cmds)
  ((image-available
    :initarg :image-available
    :initform nil
    :accessor signal-image-available)
   (render-finish
    :initarg :render-finish
    :initform nil
    :accessor signal-render-finish))
  (:documentation
   "use to store semaphores and fences type is list!!!!"))

(defclass app (vk.signal) ())

(declaim (inline app-handle))

(defun app-handle (app)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (gethash app *apps*))



