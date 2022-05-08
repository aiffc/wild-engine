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
		 ;; ./src/core
		 (:module "core"
		  :components ((:file "utils")
			       ;; ./src/core/windows
			       (:module "windows"
				:components ((:file "win")))
			       ;; ./src/core/vk
			       (:module "vk"
				:components ((:file "utils")
					     (:file "instance")
					     (:file "gpu")
					     (:file "surface")
					     (:file "device")
					     (:file "swapchain")
					     (:file "render-pass")
					     (:file "framebuffer")
					     (:file "cmd-pool")
					     (:file "cmds")
					     (:file "signal"))
					)
			       ;; ./src/core/app
			       (:module "app"
				:components ((:file "app")))))
		 ;; ./src/math
		 (:module "math")
		 ;; ./src/api
		 (:module "api"
		  :components ((:file "app"))))))
  :description ""
  :in-order-to ((test-op (test-op "wild-engine/tests"))))

(defsystem "wild-engine/tests"
  :author ""
  :license ""
  :depends-on ("wild-engine" "rove")
  :components ((:file "tests/package")
	       (:file "tests/win-test/win-test"))
  :description "Test system for wild-engine"
  :perform (test-op (op c) (symbol-call :rove :run c)))
