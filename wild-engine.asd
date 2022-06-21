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
	       :classimp)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "utils")
		 ;; ./src/dbg
		 (:module "dbg"
		  :components ((:file "dbg")
			       (:file "doc")))
		 ;; ./src/math
		 (:module "math"
		  :components ((:file "vec2")
			       (:file "vec3")
			       (:file "vec4")
			       (:file "mat2")
			       (:file "mat3")
			       (:file "mat4")
			       (:file "util")))
		 ;; ./src/core
		 (:module "core"
		  :components ((:file "utils")
			       ;; ./src/core/windows
			       (:module "windows"
				:components ((:file "win")))
			       ;; ./src/core/vk
			       (:module "vk"
			       	:components ((:file "utils")
			       		     (:file "depth-buffer")
			       		     (:file "instance")
			       		     (:file "gpu")
			       		     (:file "surface")
			       		     (:file "device")
			       		     (:file "swapchain")
			       		     (:file "render-pass")
					     (:file "pipeline-cache")
			       		     (:file "graphics-pipeline")
			       		     (:file "framebuffer")
			       		     (:file "cmd-pool")
			       		     (:file "cmds")
			       		     (:file "signal")
			       		     (:file "uniform")
			       		     (:file "texture")
			       		     (:file "descriptor")
			       		     (:file "layout")
			       		     (:file "vertex-buffer")
			       		     (:file "index-buffer")))
			       (:module "model"
				:components ((:file "model")))
			       ;; ./src/core/app
			       (:module "app"
				:components ((:file "app")))))
		 ;; ./src/api
		 (:module "api"
		  :components ((:file "app")
		 	       (:file "graphics-pipeline")
		 	       (:file "set")
		 	       (:file "uniform")
			       (:file "texture")
		 	       (:file "descriptor")
		 	       (:file "layout")
			       (:file "vertex")
			       (:file "model")
			       (:module "doc"
				:components ((:file "app")
					     (:file "graphics-pipeline")
					     (:file "set")
					     (:file "uniform")
					     (:file "texture")
					     (:file "descriptor")
					     (:file "layout")
					     (:file "vertex")
					     (:file "model"))))))))
  :description ""
  :in-order-to ((test-op (test-op "wild-engine/tests"))))

(defsystem "wild-engine/tests"
  :author ""
  :license ""
  :depends-on ("wild-engine" "rove")
  :components ((:file "tests/package")
	       (:file "tests/utils")
	       (:file "tests/win-test/win-test")
	       (:file "tests/triangle/triangle")
	       (:file "tests/vertex/vertex")
	       (:file "tests/index/index")
	       (:file "tests/uniform/uniform")
	       (:file "tests/image/image")
	       (:file "tests/model/model"))
  :description "Test system for wild-engine"
  :perform (test-op (op c) (symbol-call :rove :run c)))
