(in-package #:we.model)

(we.vk:defbuffer mesh-node (:usage :vertex)
  (vertices :vec3)
  (texture-coords :vec3)
  (normals :vec3)
  (colors :vec4))

(defun vector3->vec3 (vec3)
  (v3:make (aref vec3 0)
	   (aref vec3 1)
	   (aref vec3 2)))

(defun color4->vec4 (color4)
  (v4:make (aref color4 0)
	   (aref color4 1)
	   (aref color4 2)
	   (aref color4 4)))

(defun parse-mesh (mesh)
  (let* ((faces (ai:faces mesh))
	 (vertices (ai:vertices mesh))
	 (text-coords (aref (ai:texture-coords mesh) 0))
	 (normals (ai:normals mesh))
	 (color (ai:colors mesh))
	 (face-size (length faces))
	 (ret (make-array (* 3 face-size) :adjustable t :fill-pointer 0)))
    (loop :for i :from 0 :below face-size
	  :for face :=  (aref faces i)
	  :for face-index0 := (aref face 0)
	  :for face-index1 := (aref face 1)
	  :for face-index2 := (aref face 2)
	  :for t0 := (aref text-coords face-index0)
	  :for t1 := (aref text-coords face-index1)
	  :for t2 := (aref text-coords face-index2)
	  :finally (return-from parse-mesh (values ret face-size))
	  :do (progn (vector-push-extend (make-mesh-node :vertices (vector3->vec3 (aref vertices face-index0))
							 :texture-coords (v3:make (aref t0 0)
										  (- 1.0 (aref t0 1))
										  0.0)
							 :normals (vector3->vec3 (aref normals face-index0))
							 :colors (v4:make 1.0 0.0 0.0 0.0))
					 ret)
		     (vector-push-extend (make-mesh-node :vertices (vector3->vec3 (aref vertices face-index1))
							 :texture-coords (v3:make (aref t1 0)
										  (- 1.0 (aref t1 1))
										  0.0)
							 :normals (vector3->vec3 (aref normals face-index1))
							 :colors (v4:make 0.0 1.0 0.0 0.0))
					 ret)
		     (vector-push-extend (make-mesh-node :vertices (vector3->vec3 (aref vertices face-index2))
							 :texture-coords (v3:make (aref t2 0)
										  (- 1.0 (aref t2 1))
										  0.0)
							 :normals (vector3->vec3 (aref normals face-index2))
							 :colors (v4:make 0.0 0.0 1.0 0.0))
					 ret)))))

(defun load-mesh (path)
  (let* ((model (ai:import-into-lisp path :processing-flags '(:ai-process-preset-target-realtime-quality))))
    (let* ((ret (parse-mesh (aref (ai:meshes model) 0)))
	   (ret-indices (eval `(vector ,@(loop :for i :from 0 :below (length ret) :collect i)))))
      (values ret ret-indices))))

;; (cffi:define-foreign-library assimp
;;   (:darwin "libassimp.dylib")
;;   (:windows (:or "assimp.dll"
;;                  "libassimp.dll"))
;;   (:unix (:or "libassimp.so.5"
;;               "libassimp.so.4"
;;               "libassimp.so.3" "libassimp3.0.so"
;;               "libassimp.so")))

;; (cffi:use-foreign-library assimp)


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
;; 	(setf (aref indices i) i
;; 	      (aref mesh-data i) (make-mesh-node :vertices
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

