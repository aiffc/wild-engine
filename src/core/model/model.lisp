(in-package #:we.model)

(cffi:define-foreign-library assimp
  (:darwin "libassimp.dylib")
  (:windows (:or "assimp.dll"
                 "libassimp.dll"))
  (:unix (:or "libassimp.so.5"
              "libassimp.so.4"
              "libassimp.so.3" "libassimp3.0.so"
              "libassimp.so")))

(cffi:use-foreign-library assimp)

(defun parse-face (face-ptr)
  (cffi:with-foreign-slots ((%assimp:num-indices
			     (:pointer %assimp:indices))
			    face-ptr (:struct %assimp:face))
    (format t "~a~%" %assimp:num-indices)
    ;; (dotimes (i %assimp:num-indices)
    ;;   (format t "~a~%" (cffi:mem-aref %assimp:indices :unsigned-int i)))
    ))

(defun vertices3->vec3 (vertices-ptr)
  (cffi:with-foreign-slots ((%assimp:x
			     %assimp:y
			     %assimp:z) vertices-ptr (:struct %assimp:vector3d))
    (format t "~a ~a ~a~%" %assimp:x %assimp:y %assimp:y)
    (v3:make %assimp:x
	     %assimp:y
	     %assimp:z)))

(defun parese-mesh (ptr)
  (cffi:with-foreign-slots ((%assimp:primitive-types
			     %assimp:num-vertices
			     %assimp:num-faces
			     (:pointer %assimp:vertices)   ;; vec3
			     (:pointer %assimp:normals)    ;; vec3
			     (:pointer %assimp:tangents)
			     (:pointer %assimp:bitangents)
			     (:pointer %assimp:colors)
			     (:pointer %assimp:texture-coords)  ;; vec3
			     %assimp:num-uv-components
			     (:pointer %assimp:faces)      ;; assimp face
			     %assimp:num-bones
			     (:pointer %assimp:bones) ;; double pointer
			     %assimp:material-index
			     %assimp:name
			     %assimp:num-anim-meshes
			     (:pointer %assimp:anim-meshes)  ;; double pointer
			     %assimp:method 
			     %assimp:aabb
			     (:pointer %assimp:texture-coords-names)) ptr (:struct %assimp:mesh))
    (declare (ignore %assimp:primitive-types
		     %assimp:tangents
		     %assimp:num-faces
		     %assimp:bitangents
		     %assimp:colors
		     %assimp:num-uv-components
		     %assimp:faces
		     %assimp:num-bones
		     %assimp:bones
		     %assimp:material-index
		     %assimp:name
		     %assimp:num-anim-meshes
		     %assimp:anim-meshes
		     %assimp:method
		     %assimp:aabb
		     %assimp:texture-coords-names))
    ;;(parse-face %assimp:faces)
    (dotimes (i %assimp:num-vertices)
      (format t "vertices:")
      (vertices3->vec3 (cffi:mem-aptr %assimp:vertices '(:struct %assimp:vector3d) i))
      (format t "texture coord:")
      (vertices3->vec3 (cffi:mem-aptr %assimp:texture-coords '(:struct %assimp:vector3d) i))
      (format t "normal :")
      (vertices3->vec3 (cffi:mem-aptr %assimp:normals '(:struct %assimp:vector3d) i))
      (format t "~%"))
    (format t "~a ~a ~a ~a~%" %assimp:num-vertices
	    %assimp:vertices
	    %assimp:normals
	    %assimp:texture-coords)))


(defun load-mesh (model-path)
  "todo"
  (let ((ptr (%assimp:import-file model-path %assimp::+process-preset-target-realtime-fast+)))
    (cffi:with-foreign-slots ((%assimp:flags
			       %assimp:root-node
			       %assimp:num-meshes
			       %assimp:meshes
			       %assimp:num-materials
			       %assimp:materials
			       %assimp:num-animations
			       %assimp:animations
			       %assimp:num-textures
			       %assimp:textures
			       %assimp:num-lights
			       %assimp:lights
			       %assimp:num-cameras
			       %assimp:cameras
			       %assimp:meta-data
			       %assimp:name
			       %assimp:private) ptr (:struct %assimp:scene))
      (declare (ignore %assimp:flags
		       %assimp:root-node
		       %assimp:num-materials
		       %assimp:materials
		       %assimp:num-animations
		       %assimp:animations
		       %assimp:num-textures
		       %assimp:textures
		       %assimp:num-lights
		       %assimp:lights
		       %assimp:num-cameras
		       %assimp:cameras
		       %assimp:meta-data
		       %assimp:name
		       %assimp:private))
      (format t "~a ~a~%" %assimp:num-meshes %assimp:meshes)
      (parese-mesh %assimp:meshes)
      (cffi:foreign-free ptr))))


