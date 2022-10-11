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

(defun parese-mesh (ptr)
  (let ((vertices-num (cffi:foreign-slot-value ptr '(:struct %assimp:mesh) '%assimp::num-vertices))
	(face-num (cffi:foreign-slot-value ptr '(:struct %assimp:mesh) '%assimp::num-faces))
	(vertices (cffi:foreign-slot-value ptr '(:struct %assimp:mesh) '%assimp::vertices))
	(normals (cffi:foreign-slot-value ptr '(:struct %assimp:mesh) '%assimp::normals))
	;; (%assimp::tangents
	;;   (:pointer (:struct %assimp::vector3d)) :offset 32)
	;; (%assimp::bitangents
	;;   (:pointer (:struct %assimp::vector3d)) :offset 40)
	;; (%assimp::colors
	;;   (:pointer (:struct %assimp::color4d)) :count 8
	;; 					:offset 48)
	(text-coords (cffi:foreign-slot-value ptr '(:struct %assimp:mesh) '%assimp::texture-coords))
	(face (cffi:foreign-slot-value ptr '(:struct %assimp:mesh) '%assimp::faces)))
    (format t "~a ~a~%" vertices-num face-num)))

(defun load-mesh (model-path)
  "todo"
  (let* ((ptr (%assimp:import-file model-path %assimp::+process-preset-target-realtime-max-quality+))
	 (mesh-count (cffi:foreign-slot-value ptr '(:struct %assimp:scene) '%assimp::num-meshes))
	 (mesh-ptr (cffi:foreign-slot-value ptr '(:struct %assimp:scene) '%assimp::meshes)))
    (format t "~a ~a~%" mesh-count mesh-ptr)
    (parese-mesh mesh-ptr)
    (cffi:foreign-free ptr)))


