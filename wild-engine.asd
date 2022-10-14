(defsystem "wild-engine"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:vk
	       :sdl-vulkan
	       :sdl2
	       :cffi
	       :shaderc
	       :rtg-math
	       :opticl
	       :alexandria
	       :cl-soil
	       :classimp
	       :closer-mop
	       :defclass-std
	       :alexandria)
  :description ""
  :components ((:module "src"
		:components ((:file "package")
			     (:file "utils")
			     (:module "dbg"
			      :components ((:file "dbg")
					   (:file "doc")))
			     (:module "math"
			      :components ((:file "vec2")
					   (:file "vec3")
					   (:file "vec4")
					   (:file "mat2")
					   (:file "mat3")
					   (:file "mat4")
					   (:file "util")))
			     (:module "core"
			      :components
			      ((:module "vk"
				:components ((:file "utils")
					     (:file "handles")
					     (:file "window")
					     (:file "instance")
					     (:file "surface")
					     (:file "gpu")
					     (:file "device")
					     (:file "swapchain")
					     (:file "cmd-pool")
					     (:file "cmds")
					     (:file "synchronization-primitives")
					     (:file "color-resources")
					     (:file "depth-buffer")
					     (:file "pipeline-cache")
					     (:file "render-pass")
					     (:file "frame-buffer")
					     (:file "graphics-pipeline")
					     (:file "buffer")
					     (:file "texture")
					     (:file "descriptor")
					     (:file "init")))
			       (:module "model"
				:components ((:file "model")))))
			     (:module "control"
			      :components
			      ((:file "ctrl")
			       (:file "set"))))))
  :in-order-to ((test-op (test-op "wild-engine/tests"))))

(defsystem "wild-engine/tests"
  :author ""
  :license ""
  :depends-on ("wild-engine" "rove")
  :description "Test system for wild-engine"
  :components ((:module "test"
		:components ((:file "package")
			     (:file "util")
			     (:module "triangle"
			      :components ((:file "triangle")))
			     (:module "index"
			      :components ((:file "index")))
			     (:module "descriptor-sets"
			      :components ((:file "descriptor-sets")))
			     (:module "model"
			      :components ((:file "model"))))))
  :perform (test-op (op c) (symbol-call :rove :run c)))

