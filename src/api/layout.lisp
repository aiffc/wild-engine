(in-package :wild-engine.api)

;; (define-layout test ()
;;     ((:uniform-buffer
;;       :name aa 
;;       :binding 0
;;       :struct ((a :vec2)       ;; uniform buffer info create uniform buffer here
;; 	       (b :vec2)))
;;      (:texture
;;       :name bb
;;       :binding 1
;;       :path "~/path-to-image")
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
	 (uniform-infos (collect-key body :uniform-buffer))
	 (uniform-bodies (parse-uniform-bodies name uniform-infos))
	 (image-infos (collect-key body :texture))
	 (image-bodies (parse-image-bodies name image-infos)))
    `(progn
       (eval-when (:compile-toplevel :execute :load-toplevel))
       ,@uniform-bodies
       ,@image-bodies
       (defun ,descriptor-fun (app)
	 (%we.vk:descriptor-pool-create-info app (append (generate-uniform-descriptor-info app ',uniform-infos)
							 (generate-texture-descriptor-info app ',image-infos))))
       (defun ,layout-fun ()
	 (%we.vk:layout-create-info (append (generate-uniform-info ',uniform-infos)
					    (generate-texture-info ',image-infos)))))))
