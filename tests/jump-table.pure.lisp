(unless (gethash 'sb-c:jump-table sb-c::*backend-parsed-vops*)
  (invoke-restart 'run-tests::skip-file))

(with-test (:name :symbol-case-as-jump-table)
  ;; Assert that a prototypical example of (CASE symbol ...)
  ;; was converted to a jump table.
  (let ((c (sb-kernel:fun-code-header #'sb-debug::parse-trace-options)))
    (assert (>= (sb-kernel:code-jump-table-words c) 14))))

(with-test (:name :type-derivation)
  (assert-type
   (lambda (x)
     (declare ((member a b c d) x)
              (optimize speed))
     (case x
       (a (print 1))
       (b (print 2))
       (c (print 4))
       (d (print 3))
       (e (print 5))))
   (integer 1 4))
  (assert-type
   (lambda (x)
     (declare ((member a b c d) x)
              (optimize speed))
     (case x
       (a 1)
       (b 2)
       (c 4)
       (d 3)
       (e 5)))
   (integer 1 4))
  (assert-type
   (lambda (x)
     (case x
       (a
        (error "x"))
       ((b k)
        (if (eq x 'a)
            11
            2))
       (c 3)
       (d 4)
       (e 5))
     (eq x 'a))
   null)
  (assert-type
   (lambda (a)
     (declare ((integer 1 5) a))
     (case a
       (1 1)
       ((2 4) (print 2))
       (3 2)
       (5 3)))
   (integer 1 3))
  (assert-type
   (lambda (a)
     (declare ((integer 1 5) a))
     (case a
       (1 1)
       ((2 4) 2)
       (3 2)
       (5 3)))
   (integer 1 3)))

(with-test (:name :type-derivation-constraints)
  (assert-type
   (lambda (x)
     (declare ((not (member b)) x)
              (optimize speed))
     (unless (eq x 'a)
       (case x
         (a (print 1))
         (b (print 2))
         (c (print 3))
         (d (print 4))
         (e (print 6))
         (g (print 5)))))
   (or null (integer 3 6)))
  (assert-type
   (lambda (x)
     (case x
       (a
        (if (eq x 'a)
            1
            10))
       ((b k)
        (if (eq x 'a)
            11
            2))
       (c 3)
       (d 4)
       (e 5)
       (t (if (eq x 'd)
              30
              6))))
   (integer 1 6)))

(defstruct a)
(defstruct (achild (:include a)))
(defstruct (agrandchild (:include achild)))
(defstruct (achild2 (:include a)))
(defstruct b)
(defstruct c)
(defstruct d)
(defstruct e)
(defstruct (echild (:include e)))
(defstruct f)

(declaim (freeze-type a b c d e f))
(defun typecase-jump-table (x)
  (typecase x
    (a 'is-a)
    (b 'is-b)
    (c 'is-c)
    ((or d e) 'is-d-or-e)
    (f 'is-f)))
(compile 'typecase-jump-table)

(with-test (:name :typecase-jump-table)
  (assert (eql (sb-kernel:code-jump-table-words
                (sb-kernel:fun-code-header #'typecase-jump-table))
               ;; 6 cases including NIL return, plus the size
               7)))

(with-test (:name :duplicates)
  (checked-compile-and-assert
      ()
      `(lambda (c)
         (position c "aaaaa"))
    ((#\a) 0)
    ((#\b) nil)))

(with-test (:name :array-subtype-dispatch-table)
  (assert (> (sb-kernel:code-jump-table-words
              (sb-kernel:fun-code-header #'sb-kernel:vector-subseq*))
             20)))

(with-test (:name :cleanups)
  (checked-compile-and-assert
   ()
   `(lambda (b c &optional f)
      (block b
        (case
            (let ((* b))
              (if (eql c 0)
                  (return-from b (funcall f 11))
                  b))
          (t (case c
               ((197 97 399) b)
               (t 0))))))
   ((33 0 (lambda (x) (+ x *))) 44)
   ((1 1) 0)
   ((2 197) 2)
   ((3 97) 3)
   ((4 399) 4)))
