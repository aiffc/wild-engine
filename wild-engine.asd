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
  :description ""
  :in-order-to ((test-op (test-op "wild-engine/tests"))))

(defsystem "wild-engine/tests"
  :author ""
  :license ""
  :depends-on ("wild-engine" "rove")
  :description "Test system for wild-engine"
  :perform (test-op (op c) (symbol-call :rove :run c)))
