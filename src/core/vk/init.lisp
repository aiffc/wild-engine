(in-package :%wild-engine.core.vk)

(defun vk->init-all (w h x y title format)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((sys (gensym)))
    (vk->init-window sys w h x y title)
    (vk->init-instance sys)
    (vk->init-surface sys)
    (vk->init-gpu sys)
    (vk->init-device sys)
    (vk->init-swapchain sys format)
    (vk->init-cmd-pool sys)
    (vk->alloc-cmds sys)
    (vk->init-synchronization-primitives sys)
    (vk->init-depth-stencil sys w h)
    (vk->init-pipeline-cache sys)
    (vk->init-render-pass sys)
    (vk->init-frame-buffer sys w h)
    (set-current-gcmd-index sys 0)
    sys))

(defun free-resource ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  ;; (destroy-all-triangle-pipeline)
  ;; (destroy-all-buffer)
  )

(defun vk->destroy-all (sys)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (free-resource)
  (vk->destroy-frame-buffer sys)
  (vk->destroy-render-pass sys)
  (vk->destroy-pipeline-cache sys)
  (vk->destroy-depth-stencil sys)
  (vk->destroy-synchronization-primitives sys)
  (vk->free-cmds sys)
  (vk->destroy-cmd-pool sys)
  (vk->destroy-swapchain sys)
  (vk->destroy-device sys)
  (vk->destroy-gpu sys)
  (vk->destroy-surface sys)
  (vk->destroy-instance sys)
  (vk->destroy-window sys))

