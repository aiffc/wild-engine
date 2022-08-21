(in-package #:wild-engine.debug)

(setf (documentation '*trace-layer* 'symbol)
      "used to store whitch layer should debug")

(setf (documentation 'msg 'function)
      "function used to print debug info")

(setf (documentation 'dbg-trace 'function)
      "function used to set whitch layer you want to debug")

(setf (documentation 'dbg-untrace 'function)
      "function used to set unset layer you had setted")
