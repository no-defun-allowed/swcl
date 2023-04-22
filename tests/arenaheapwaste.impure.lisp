#+(or interpreter (not system-tlabs) (not sb-thread))
(invoke-restart 'run-tests::skip-file)

(setf (generation-number-of-gcs-before-promotion 0) 1000000)

(defun use-heap-and-arena ()
  (values (make-array 100)
          (sb-vm:without-arena "test"
            (make-array 100))))

(defvar *runflag* t)
(defun worker (index arena semaphores)
  (declare (ignorable index))
  (loop
    (sb-thread:wait-on-semaphore (first semaphores))
    (unless *runflag* (return))
    (sb-vm:with-arena (arena)
      (dotimes (i 1000)
        (use-heap-and-arena)))
    (sb-thread:signal-semaphore (second semaphores))))

(defun try-wasting-heap (&optional (nthreads 4))
  (let* ((arena (sb-vm:new-arena (* 10 1024 1024)))
         (semaphores (list (sb-thread:make-semaphore)
                           (sb-thread:make-semaphore)))
         (threads
          (loop repeat 4
                for i from 0
                collect
                (sb-thread:make-thread #'worker :arguments (list i arena semaphores))))
         (niter 500)
         (sum-fractional-waste 0))
    (dotimes (i niter)
      (sb-thread:signal-semaphore (first semaphores) nthreads)
      (sb-thread:wait-on-semaphore (second semaphores) :n nthreads)
      (let* ((used-bytes (generation-bytes-allocated 0))
             (consumed-bytes
              (* (alien-funcall (extern-alien "count_generation_pages" (function long char unsigned))
                                0 0)
                 sb-vm:gencgc-page-bytes))
             (waste-bytes (- consumed-bytes used-bytes))
             (waste (/ waste-bytes consumed-bytes)))
        (incf sum-fractional-waste waste)
        #+nil (format t "~&waste: ~,,2f~%" waste))
      (sb-vm:rewind-arena arena))
    (setq *runflag* nil)
    (sb-thread:signal-semaphore (first semaphores) nthreads)
    (mapc 'sb-thread:join-thread threads)
    (/ sum-fractional-waste niter)))

(with-test (:name :waste-heap)
  ;; Prior to the logic that picked up where we left off in the heap
  ;; it was easily demonstrated that the heap waste could rise to as much
  ;; as 70% before starting a GC.
  (let ((avg-frac-waste (try-wasting-heap)))
    (assert (< avg-frac-waste .05))))
