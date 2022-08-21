(in-package :we.test)

;; (define-traignle-vertex vertex ()
;;   (make-triangle-vertex :v #(-0.5 -0.5 0.0) :vt #(0.5 0.5 0.5))
;;   (make-triangle-vertex :v #(0.5 -0.5 0.0) :vt #(0.0 0.5 0.5))
;;   (make-triangle-vertex :v #(0.5 0.5 0.0) :vt #(0.5 0.0 0.5))
;;   (make-triangle-vertex :v #(-0.5 0.5 0.0) :vt #(0.5 0.5 0.0)))

;; (define-index index ()
;;   0 1 2 2 3 0)
(defun test ()
  (with-we-init (sys :w 800 :h 800 :x 0 :y 0 :title "test")
    (with-we-main-loop ()
      (with-gcmd (cmd sys 0 0 800 800 #(1.0 1.0 1.0 1.0))))))


