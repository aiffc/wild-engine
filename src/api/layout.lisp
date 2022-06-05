(in-package :wild-engine.api)

;; (define-layout test
;;     ((:uniform-buffer
;;       :name aa 
;;       :binding 0
;;       :struct ((a :vec2)       ;; uniform buffer info create uniform buffer here
;; 	       (b :vec2)))
;;      (:texture
;;       :name bb
;;       :binding 1
;;       :image-info (:image-w 600     ;; image info create image handle and info here
;; 		   :image-h 600))
;;      (:uniform-buffer
;;       :name cc
;;       :binding 2
;;       :struct ((a :vec2)
;; 	       (b :vec3)
;; 	       (c :vec2)))))

(defun collect-key (body key)
  (loop :for bd :in body
	:when (eql (first bd) key)
	  :collect bd))

(defmacro define-layout (name () (&rest body))
  "ready to do"
  (let* ((layout-fun (we.u:create-symbol 'layout- name))
	 (descriptor-fun (we.u:create-symbol 'descriptor- name))
	 (uniform-buffers (collect-key body :uniform-buffer))
	 (uniform-bodies (parse-uniform-bodies name uniform-buffers)))
    `(progn
       (eval-when (:compile-toplevel :execute :load-toplevel))
       ,@uniform-bodies
       (defun ,descriptor-fun (app)
	 (%we.vk:descriptor-pool-create-info app (generate-uniform-descriptor-info app ',uniform-buffers)))
       (defun ,layout-fun ()
	 (%we.vk:layout-create-info (generate-uniform-info ',uniform-buffers))))))
