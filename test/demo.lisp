(defpackage :we.demo
  (:use #:cl)
  (:export #:run))

(in-package #:we.demo)

(defun run (model)
  (case model
    (:triangle (triangle:triangle))
    (:indices (index:index))
    (:descriptor-sets (descriptor-sets:descriptor-sets))
    (:model (model:model))
    (:dynamic-uniform (dynamic-uniform:dynamic-uniform))
    (t (error "unknow demo or not support~~~~~~ ~%"))))
