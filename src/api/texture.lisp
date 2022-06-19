(in-package :wild-engine.api)

(defparameter *texture-hash* (make-hash-table))
(defparameter *texture-info-hash* (make-hash-table))

;; (defmacro define-texture ((name path) &key
;; 					(layout :shader-read-only-optimal)
;; 					(width 0)
;; 					(height 0)
;; 					(mip-level 1)
;; 					(depth 1)
;; 					(rformat :rgba)
;; 					(format :r8g8b8a8-srgb)
;; 					(tiling :optimal)
;; 					(usage '(:transfer-src :transfer-dst :sampled))
;; 					(samples :1)
;; 					(propeties :device-local))
;;   (let ((texture-fun (u:create-symbol name '-texture))
;; 	(texture-create-fun (u:create-symbol 'create-texture- name))
;; 	(texture-info-fun (u:create-symbol name '-image-info)))
;;     `(progn
;;        (eval-when (:compile-toplevel :load-toplevel :execute))
;;        (defun ,texture-info-fun (app)
;; 	 (let ((info (core:get-texture-info app ',name)))
;; 	   (setf (vk:image-layout info) ,layout)
;; 	   (list ',name
;; 		 info)))
;;        (defun ,texture-fun (w h)
;; 	 (core:create-image-properties ,(if (= width 0) `w width)
;; 				       ,(if (= height 0) `h height)
;; 				       ,mip-level
;; 				       ,depth
;; 				       ,format
;; 				       ,tiling
;; 				       ',usage
;; 				       ,samples
;; 				       ,propeties))
;;        (defun ,texture-create-fun (app)
;; 	 (values (core:create-texture app ',name ,path #',texture-fun ,rformat))))))

(defun parse-image-body (name body
			 &aux
			   (info (rest body)))
  (let* ((image-name (getf info :name))
	 (image-path (getf info :path))
	 (format (getf info :format))
	 (texture-layout (we.u:set-value info :layout :shader-read-only-optimal))
	 (texture-binding (getf info :binding))
	 (texture-fun (we.u:create-symbol image-name '-texture))
	 (texture-create-fun (we.u:create-symbol 'create-texture- image-name))
	 (texture-info-fun (we.u:create-symbol image-name '-texture-info))
	 (mip-levels (we.u:set-value info :mip-levels 1))
	 (tiling (we.u:set-value info :tiling :optimal))
	 (usage (we.u:set-value info :usage '(:transfer-src :transfer-dst :sampled)))
	 (samples (we.u:set-value info :samples :1))
	 (propeties (we.u:set-value info :propeties :device-local)))
    (pushnew texture-create-fun (gethash name *texture-hash*))   ;; store texture initialize functions
    (pushnew texture-info-fun (gethash name *texture-info-hash*))   ;; store texture initialize functions
    `(progn
       (defun ,texture-info-fun (app)
	 (let ((info (%we.vk:get-texture-info app ',image-name)))
	   (setf (vk:image-layout info) ,texture-layout)
	   (values ',name info ,texture-binding)))
       (defun ,texture-fun ()
	 (%we.vk:create-image-properties
	  :mip-levels ,mip-levels
	  :tiling ,tiling
	  :usage ',usage
	  :samples ,samples
	  :propeties ,propeties))
       (defun ,texture-create-fun (app)
	 (%we.vk:create-texture app ',image-name ,image-path #',texture-fun ,format)))))

(defun parse-image-bodies (name image-infos)
  (loop :for info :in image-infos
	:collect (parse-image-body name info)))

(defun build-texture-info (arg
			   &aux (uarg (rest arg)))
  "generate a descriptor set create info for uniform buffer"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (vk:make-descriptor-set-layout-binding
   :binding (getf uarg :binding)
   :descriptor-type :combined-image-sampler
   :descriptor-count (we.u:set-value uarg :count 1)
   :stage-flags :fragment
   :immutable-samplers nil))          ;; not support yet

(defun generate-texture-info (args)
  "generate uniform buffer infos for descriptor layouts"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop :for arg :in args
	:collect (build-texture-info arg)))
