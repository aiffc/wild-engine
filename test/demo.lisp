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
    (:multi-pipeline (multi-pipeline:multi-pipeline))
    (:push-constant (push-constant:push-constant))
    (t (error "unknow demo or not support~~~~~~ ~%"))))
