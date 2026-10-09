(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defun compact-draw-lists (dpy rm-draw-data releaseme-queue)
  (declare (ignore dpy))

  (restart-bind ((ignore (lambda (&optional c)
			   (declare (ignorable c))
			   (throw :ignore nil))))
    (catch :ignore
      
      (with-slots (2d-point-list-draw-list
		   2d-line-list-draw-list
		   2d-triangle-list-draw-list
		   2d-triangle-list-draw-list-for-text
		   3d-point-list-draw-list
		   3d-line-list-draw-list
		   3d-triangle-list-draw-list
		   3d-triangle-list-with-normals-draw-list
		   2d-line-strip-draw-list
		   2d-triangle-strip-draw-list
		   3d-line-strip-draw-list
		   3d-triangle-strip-draw-list
		   3d-triangle-strip-with-normals-draw-list) rm-draw-data
    
	(setf 2d-point-list-draw-list (compact-draw-list-group 2d-point-list-draw-list releaseme-queue)
	      2d-line-list-draw-list (compact-draw-list-group 2d-line-list-draw-list releaseme-queue)
	      2d-triangle-list-draw-list (compact-draw-list-group 2d-triangle-list-draw-list releaseme-queue)
	      2d-triangle-list-draw-list-for-text (compact-draw-list-group 2d-triangle-list-draw-list-for-text releaseme-queue)
	      3d-point-list-draw-list (compact-draw-list-group 3d-point-list-draw-list releaseme-queue)
	      3d-line-list-draw-list (compact-draw-list-group 3d-line-list-draw-list releaseme-queue)
	      3d-triangle-list-draw-list (compact-draw-list-group 3d-triangle-list-draw-list releaseme-queue)
	      3d-triangle-list-with-normals-draw-list (compact-draw-list-group 3d-triangle-list-with-normals-draw-list releaseme-queue)
	      2d-line-strip-draw-list (compact-draw-list-group 2d-line-strip-draw-list releaseme-queue)
	      2d-triangle-strip-draw-list (compact-draw-list-group 2d-triangle-strip-draw-list releaseme-queue)
	      3d-line-strip-draw-list (compact-draw-list-group 3d-line-strip-draw-list releaseme-queue)
	      3d-triangle-strip-draw-list (compact-draw-list-group 3d-triangle-strip-draw-list releaseme-queue)
	      3d-triangle-strip-with-normals-draw-list (compact-draw-list-group 3d-triangle-strip-with-normals-draw-list releaseme-queue))

	;; wow. i'm not compacting any of the draw lists in tables. who'da thunk.
    
	(values)))))

(defun copy-vertex (old-vertex-array old-vertex-offset new-vertex-array new-vertex-offset vertex-type-size-bytes)
  (let ((vertex-size-in-uints (ash vertex-type-size-bytes -2))
	(new-lisp-array (foreign-array-bytes new-vertex-array))
	(old-lisp-array (foreign-array-bytes old-vertex-array)))
    ;;(print "-----------")
    (loop for i from (* new-vertex-offset vertex-size-in-uints)
       for j from (* old-vertex-offset vertex-size-in-uints)
       repeat vertex-size-in-uints
       do (setf (aref new-lisp-array i) (aref old-lisp-array j))))
  ;;(print "-----------")
  ;;(finish-output)
  (values))

(defun draw-list-needs-compaction? (draw-list)
  (let ((cmd-vector (draw-list-cmd-vector draw-list))
	(num-deleted (draw-list-num-deleted draw-list)))
    (and cmd-vector num-deleted
	 (> (car num-deleted) (floor (* (fill-pointer cmd-vector) *compact-trigger*))))))

(defun compact-draw-list-group (draw-list releaseme-queue)
  (unless (draw-list-needs-compaction? draw-list)
    (return-from compact-draw-list-group draw-list))
  (compact-draw-list-group-1 draw-list releaseme-queue))

(defun release-device-memory-draw-list-group (draw-list releaseme-queue)
  (do ((dl draw-list (draw-list-prev draw-list)))
      ((null dl))
    (let ((im (draw-list-index-memory dl))
	  (vm (draw-list-vertex-memory dl)))
      (when im
	(lparallel.queue:push-queue
	 #'(lambda ()
	     (release-memory im))
	 releaseme-queue))
      (when vm
	(lparallel.queue:push-queue
	 #'(lambda ()
	     (release-memory vm))
	 releaseme-queue)))))

(defun compact-draw-list-group-1 (draw-list releaseme-queue)
  (let* ((orig-cmd-vector (draw-list-cmd-vector draw-list))
	 (copy (copy-seq orig-cmd-vector)))
    
    (unless orig-cmd-vector
      (return-from compact-draw-list-group-1 draw-list))
    
    (when (= (fill-pointer orig-cmd-vector) 0)
      (return-from compact-draw-list-group-1 draw-list))

    (unwind-protect
	 (let ((loc (make-instance (class-of draw-list))))
	   (loop for cmd across orig-cmd-vector
		 unless (cmd-deleted? cmd)
		   do (let* ((old-draw-list (cmd-draw-list cmd))
			     (old-index-array (draw-list-index-array old-draw-list))
			     (old-vertex-array (draw-list-vertex-array old-draw-list))
			     (vertex-type-size (foreign-array-foreign-type-size old-vertex-array))
			     (vertex-type-size-uint (ash vertex-type-size -2)))
		 
			(with-next-draw-list (new-draw-list
					      new-cmd-vector
					      loc
					      (make-instance (class-of draw-list)))

			  (let* ((new-index-array (draw-list-index-array new-draw-list))
				 (new-vertex-array (draw-list-vertex-array new-draw-list))
				 (new-first-idx (foreign-array-fill-pointer new-index-array))
				 (new-vtx-offset (foreign-array-fill-pointer new-vertex-array)))
		     
			    (loop repeat (cmd-elem-count cmd)
				  for i from (cmd-first-idx cmd)
				  with old-vtx-offset = (cmd-vtx-offset cmd)
				  with seen = ()
				  do (let* ((local-offset (aref (foreign-array-bytes old-index-array) i))
					    (key (list old-index-array local-offset)))

				       (index-array-push-extend new-index-array local-offset)
				  
				       (unless (member key seen :test #'equalp) ;; no vertex should be copied twice
				    
					 (with-slots (bytes fill-pointer allocated-count) new-vertex-array
					   (let ((reqd-count (+ 1 new-vtx-offset local-offset))
						 (alloc-count allocated-count))
					     (when (> reqd-count alloc-count)
					       (let ((new-count (* 2 reqd-count)))
						 (declare (type fixnum new-count))
						 (let ((new-array (make-array (* new-count vertex-type-size-uint)
									      :element-type '(unsigned-byte 32)))
						       (old-array bytes))
						   #+SBCL(sb-sys:with-pinned-objects (new-array old-array)
							   (memcpy (sb-sys:vector-sap new-array)
								   (sb-sys:vector-sap old-array)
								   (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
								      (cl:the (integer 0 512) vertex-type-size))))
	      
						   #+ALLEGRO
						   (loop for i from 0 below (* fill-pointer vertex-type-size-uint)
							 do (setf (aref new-array i) (aref old-array i)))
					      
						   #+CCL
						   (ccl::%copy-ivector-to-ivector
						    old-array 0 new-array 0
						    (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
						       (cl:the (integer 0 512) vertex-type-size)))
					      
						   (setf bytes new-array)
						   (setf allocated-count new-count))))

					     (setf fill-pointer (max fill-pointer reqd-count))))
					
					 ;; copy one vertex into new vertex array
					 (copy-vertex old-vertex-array (+ old-vtx-offset local-offset)
						      new-vertex-array (+ new-vtx-offset local-offset)
						      vertex-type-size)
				    
					 ;; we've now seen this vertex
					 (push key seen)))
				
				  finally ;; update the cmd with the new-draw-list, new-first-idx and new-vtx-offset
					  ;; we recycle cmd objects because they exist in the primitive handle hash table
					  ;; and we don't want to have to fix those relationships
					  (setf (cmd-draw-list cmd) new-draw-list)
					  (setf (cmd-first-idx cmd) new-first-idx)
					  (setf (cmd-vtx-offset cmd) new-vtx-offset)
					  (vector-push-extend cmd new-cmd-vector)))))
		 
		 finally (return loc)))

      (release-device-memory-draw-list-group draw-list releaseme-queue)
      (assert (loop for c1 across orig-cmd-vector
		    for c2 across copy
		    unless (eq c1 c2)
		      do (return nil)
		    finally (return t)))
      )))


	

      
