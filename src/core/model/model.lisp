(in-package #:we.model)

;; (cffi:define-foreign-library assimp
;;   (:darwin "libassimp.dylib")
;;   (:windows (:or "assimp.dll"
;;                  "libassimp.dll"))
;;   (:unix (:or "libassimp.so.5"
;;               "libassimp.so.4"
;;               "libassimp.so.3" "libassimp3.0.so"
;;               "libassimp.so")))

;; (cffi:use-foreign-library assimp)

;; (we.vk:defbuffer mesh-node (:usage :vertex)
;;   (vertices :vec3)
;;   (texture-coords :vec3)
;;   (normals :vec3)
;;   (colors :vec4))

;; (defun vector3->vec3 (vertices-ptr)
;;   (cffi:with-foreign-slots ((%assimp:x
;; 			     %assimp:y
;; 			     %assimp:z) vertices-ptr (:struct %assimp:vector3d))
;;     (v3:make %assimp:x %assimp:y %assimp:z)))

;; (defun color4->vec4 (color-ptr)
;;   (cffi:with-foreign-slots ((%assimp:r
;; 			     %assimp:g
;; 			     %assimp:b
;; 			     %assimp:a) color-ptr (:struct %assimp:color4d))
;;     (v4:make %assimp:r %assimp:g %assimp:b %assimp:a)))

;; (defun parse-face (face-ptr)
;;   (cffi:with-foreign-slots ((%assimp:num-indices
;; 			     (:pointer %assimp:indices))
;; 			    face-ptr (:struct %assimp:face))
;;     (format t "~a~%" %assimp:num-indices)
;;     ;; (dotimes (i %assimp:num-indices)
;;     ;;   (format t "~a~%" (cffi:mem-aref %assimp:indices :unsigned-int i)))
;;     ))

;; (defun parese-mesh (ptr)
;;   (cffi:with-foreign-slots ((%assimp:primitive-types
;; 			     %assimp:num-vertices
;; 			     %assimp:num-faces
;; 			     (:pointer %assimp:vertices)   ;; vec3
;; 			     (:pointer %assimp:normals)    ;; vec3
;; 			     (:pointer %assimp:tangents)
;; 			     (:pointer %assimp:bitangents)
;; 			     (:pointer %assimp:colors)     ;; vec3
;; 			     (:pointer %assimp:texture-coords)  ;; vec3
;; 			     %assimp:num-uv-components
;; 			     (:pointer %assimp:faces)      ;; assimp face
;; 			     %assimp:num-bones
;; 			     (:pointer %assimp:bones) ;; double pointer
;; 			     %assimp:material-index
;; 			     %assimp:name
;; 			     %assimp:num-anim-meshes
;; 			     (:pointer %assimp:anim-meshes)  ;; double pointer
;; 			     %assimp:method 
;; 			     %assimp:aabb
;; 			     (:pointer %assimp:texture-coords-names)) ptr (:struct %assimp:mesh))
;;     (declare (ignore %assimp:primitive-types
;; 		     %assimp:tangents
;; 		     %assimp:bitangents
;; 		     %assimp:num-uv-components
;; 		     %assimp:num-bones
;; 		     %assimp:bones
;; 		     %assimp:material-index
;; 		     %assimp:name
;; 		     %assimp:num-anim-meshes
;; 		     %assimp:anim-meshes
;; 		     %assimp:method
;; 		     %assimp:aabb
;; 		     %assimp:texture-coords-names))
;;     (format t "~a~%" %assimp:num-faces)
;;     (parse-face %assimp:faces)
;;     (let ((mesh-data (make-array %assimp:num-vertices))
;; 	  (indices (make-array %assimp:num-vertices)))
;;       (dotimes (i %assimp:num-vertices (values mesh-data indices))
;; 	(setf (svref indices i) i
;; 	      (svref mesh-data i) (make-mesh-node :vertices
;; 						  (vector3->vec3 (cffi:mem-aptr %assimp:vertices '(:struct %assimp:vector3d) i))
;; 						  :texture-coords
;; 						  (vector3->vec3 (cffi:mem-aptr %assimp:texture-coords '(:struct %assimp:vector3d) i))
;; 						  :normals
;; 						  (vector3->vec3 (cffi:mem-aptr %assimp:normals '(:struct %assimp:vector3d) i))
;; 						  :colors
;; 						  (color4->vec4 (cffi:mem-aptr %assimp:colors '(:struct %assimp:color4d) i))))))))


;; (defun load-mesh (model-path)
;;   "todo"
;;   (let ((ptr (%assimp:import-file model-path %assimp:+process-preset-target-realtime-max-quality+)))
;;     (cffi:with-foreign-slots ((%assimp:flags
;; 			       (:pointer %assimp:root-node)
;; 			       %assimp:num-meshes
;; 			       (:pointer %assimp:meshes)
;; 			       %assimp:num-materials
;; 			       (:pointer %assimp:materials)
;; 			       %assimp:num-animations
;; 			       (:pointer %assimp:animations)
;; 			       %assimp:num-textures
;; 			       (:pointer %assimp:textures)
;; 			       %assimp:num-lights
;; 			       (:pointer %assimp:lights)
;; 			       %assimp:num-cameras
;; 			       (:pointer %assimp:cameras)
;; 			       (:pointer %assimp:meta-data)
;; 			       (:pointer %assimp:name)
;; 			       %assimp:private) ptr (:struct %assimp:scene))
;;       (declare (ignore %assimp:flags
;; 		       %assimp:root-node
;; 		       %assimp:num-materials
;; 		       %assimp:materials
;; 		       %assimp:num-animations
;; 		       %assimp:animations
;; 		       %assimp:num-textures
;; 		       %assimp:textures
;; 		       %assimp:num-lights
;; 		       %assimp:lights
;; 		       %assimp:num-cameras
;; 		       %assimp:cameras
;; 		       %assimp:meta-data
;; 		       %assimp:name
;; 		       %assimp:private))
;;       (multiple-value-bind (mesh-data indices-data) (parese-mesh %assimp:meshes)
;; 	(cffi:foreign-free ptr)
;; 	(values mesh-data indices-data)))))

