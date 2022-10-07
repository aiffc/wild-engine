(in-package :%wild-engine.core.vk)

(defmacro defdescriptor (name (max-sets) &body body)
  "
usage ->
   (defdescriptor descriptor (1)
     (:type :uniform-buffer
      :count 1)
     (:type :combined-image-sampler
      :count 1))
usage export 
  createdp-*name*
  destroydp-*name*
  withdp-*name*
"
  (let ((pool-size-info (we.u:create-symbol name '-descriptor-pool-size))
	(pool-create-fun (we.u:create-symbol 'makedp- name))
	(pool-destroy-fun (we.u:create-symbol 'destroydp- name))
	(with-pool (we.u:create-symbol 'withdp- name)))
    `(progn
       (eval-when (:execute :load-toplevel :compile-toplevel))
       (defun ,pool-size-info ()
	 (list ,@ (loop :for bd :in body
			:collect `(vk:make-descriptor-pool-size
				   :type ,(getf bd :type)
				   :descriptor-count ,(getf bd :count)))))
       (defun ,pool-create-fun (sys
				&aux (device (get-device sys)))
	 (let* ((create-info (vk:make-descriptor-pool-create-info
			      :max-sets ,max-sets
			      :pool-sizes (,pool-size-info)))
		(pool (vk:create-descriptor-pool device create-info)))
	   (we.dbg:msg :app "create descriptor pool ~a ~a~%" ',pool-create-fun pool)
	   pool))
       (defun ,pool-destroy-fun (sys pool
				 &aux (device (get-device sys)))
	 (we.dbg:msg :app "destroy descriptor pool ~a ~a~%" ',pool-destroy-fun pool)
	 (vk:destroy-descriptor-pool device pool))
       (defmacro ,with-pool ((pool sys) &body wbody)
	 (let ((pool-create-fun (we.u:create-symbol 'makedp- ',name))
	       (pool-destroy-fun (we.u:create-symbol 'destroydp- ',name)))
	   `(let ((,pool (,pool-create-fun ,sys)))
	      (progn ,@wbody)
	      (,pool-destroy-fun ,sys ,pool)))))))

(defmacro defdescriptor-sets (name &optional (count 1))
  "
usage ->
  (defdescriptor-sets set0 1)
usage export 
   allocds-*name*
   freeds-*name*
   updateds-buffer-*name*
   updateds-image-*name*
   withds-*name*
"
  (let ((alloc-fun (we.u:create-symbol 'allocds- name))
	(free-fun (we.u:create-symbol 'freeds- name))
	(update-buffer-fun (we.u:create-symbol 'updateds-buffer- name))
	(update-image-fun (we.u:create-symbol 'updateds-image- name))
	(with-sets (we.u:create-symbol 'withds- name)))
    `(progn
       (eval-when (:execute :load-toplevel :compile-toplevel))
       (defun ,alloc-fun (sys pool layouts
			  &aux (device (get-device sys)))
	 (let* ((alloc-info (vk:make-descriptor-set-allocate-info
			     :descriptor-pool pool
			     :set-layouts (loop :for i :from 0 :below ,count
						:collect (first layouts))))
		(sets (vk:allocate-descriptor-sets device alloc-info)))
	   (we.dbg:msg :app "allocate descriptor sets ~a ~a~%" ',alloc-fun pool)
	   sets))
       (defun ,free-fun (sys pool sets
			 &aux (device (get-device sys)))
	 (we.dbg:msg :app "free descriptor sets ~a ~a~%" ',free-fun pool)
	 (vk:free-descriptor-sets device pool sets))
       (defun ,update-buffer-fun (sys sets buffer
				  &key
				    (index 0)
				    (offset 0)
				    (range %vk:+whole-size+)
				    (binding 0)
				    (array-element 0)
				    (type :uniform-buffer)
				  &aux (device (get-device sys)))
	 ;;(format t "~a~%" (we.vk::get-uniform-buffer-by-index buffer index))
	 (let* ((buffer-info (vk:make-descriptor-buffer-info
			      :buffer (we.vk::get-uniform-buffer-by-index buffer index)
			      :offset offset
			      :range range))
		(write (vk:make-write-descriptor-set
			:dst-set (nth index sets) 
			:dst-binding binding
			:dst-array-element array-element
			:descriptor-type type
			:buffer-info (list buffer-info))))
	   (we.dbg:msg :app "write buffer ~a to sets ~a ~%" buffer (nth index sets))
	   (vk:update-descriptor-sets device (list write) nil)))
       (defun ,update-image-fun (sys sets image
				 &key
				   (index 0)
				   (binding 0)
				   (array-element 0)
				 &aux (device (get-device sys)))
	 (let* ((image-info (vk:make-descriptor-image-info
			     :sampler (we.vk::texture-sampler image)
			     :image-view (we.vk::texture-view image)
			     :image-layout :shader-read-only-optimal))
		(write (vk:make-write-descriptor-set
			:dst-set (nth index sets) 
			:dst-binding binding
			:dst-array-element array-element
			:descriptor-type :combined-image-sampler
			:image-info (list image-info))))
	   (we.dbg:msg :app "write image ~a to sets ~a ~%" image (nth index sets))
	   (vk:update-descriptor-sets device (list write) nil)))
       (defmacro ,with-sets ((sets sys pool layouts) &body wbody)
	 (let ((alloc-fun (we.u:create-symbol 'allocds- ',name))
	       (free-fun (we.u:create-symbol 'freeds- ',name)))
	   `(let ((,sets (,alloc-fun ,sys ,pool ,layouts)))
	      (progn ,@wbody)
	      (,free-fun ,sys ,pool ,sets)))))))
