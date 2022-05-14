(defpackage :we.test.utils
  (:use #:cl)
  (:nicknames #:we.tu)
  (:local-nicknames)
  (:export
   #:*triangle-frag*
   #:*triangle-vert*
   #:*vertex-frag*
   #:*vertex-vert*))

(defpackage :we.win-test
  (:use #:cl)
  (:nicknames #:win-test)
  (:local-nicknames
   (:wild-engine.api #:we.api))
  (:export
   #:win-test))

(defpackage :we.triangle
  (:use #:cl)
  (:nicknames #:triangle)
  (:local-nicknames
   (:wild-engine.api #:we.api)
   (:we.test.utils #:we.tu))
  (:export
   #:triangle))

(defpackage :we.vertex
  (:use #:cl)
  (:nicknames #:vertex)
  (:local-nicknames
   (:wild-engine.api #:we.api)
   (:we.test.utils #:we.tu))
  (:export
   #:vertex))
