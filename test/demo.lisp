(defpackage :we.demo
  (:use #:cl)
  (:export #:run))

(in-package #:we.demo)

(defun run (model &rest args &key &allow-other-keys)
  (case model
    (:triangle (triangle:triangle))
    (:indices (index:index))
    (:descriptor-sets (descriptor-sets:descriptor-sets))
    (:model (model:model))
    (:dynamic-uniform (dynamic-uniform:dynamic-uniform))
    (:multi-pipeline (multi-pipeline:multi-pipeline))
    (:push-constant (push-constant:push-constant))
    (:specialization-constants (apply #'specialization-constants:specialization-constants args))
    (t (error "unknow demo or not support~~~~~~ ~%"))))
