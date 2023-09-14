;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;;; This file of tests was added because the tests in 'compiler.pure.lisp'
;;;; are a total hodgepodge- there is often no hugely compelling reason for
;;;; their being tests of the compiler per se, such as whether
;;;; INPUT-ERROR-IN-COMPILED-FILE is a subclass of SERIOUS-CONDITION;
;;;; in addition to which it is near impossible to wade through the
;;;; ton of nameless, slow, and noisy tests.

#+sb-unicode
(with-test (:name :base-char-p)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (if (sb-kernel:base-char-p x)
                  (characterp x)
                  t))))
          '(function (t) (values (member t) &optional)))))

(with-test (:name :setq-eql)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x) (let (y) (setq y x) (eql y x)))))
          '(function (t) (values (member t) &optional)))))

(with-test (:name :setq-lvar-substition)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare ((integer 0 10) a)
                  (fixnum b))
         (let ((c b))
           (setq b a)
           (eql c b)))
    ((0 2) nil)
    ((0 0) t)))

(with-test (:name :number-comparisons)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (a)
              (if (< a 0)
                  (typep a '(integer 0 10))
                  nil))))
          '(function (t) (values null &optional))))
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (a)
              (if (= a 30)
                  (typep a '(integer 0 10))
                  nil))))
          '(function (t) (values null &optional)))))

(with-test (:name :=-constraint-complex-no-bounds)
  (checked-compile-and-assert
      ()
      `(lambda (p)
        (let ((x #c(1 2)))
          (when (= x p)
            x)))
    ((#c(1 2)) #c(1 2))
    ((#c(2 1)) nil)))

(with-test (:name :compare-both-operands)
  (checked-compile-and-assert
      ()
      `(lambda (a b)
         (declare (type real a b))
         (if (>= a a)
             (if (= b a)
                 1
                 2)
             t))
    ((0 1) 2)
    ((1 1) 1)))

(with-test (:name :eql-constant)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (i)
                     (declare ((integer 0) i))
                     (cond
                       ((= i 0) 3)
                       ((= i 1) 3)
                       (t i))))))
          '(values (integer 2) &optional))))

(with-test (:name :ir1-phases-delay)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (n z)
                     (when (typep n 'fixnum)
                       (let ((ar (if (integerp n)
                                     (make-array n)
                                     z)))
                         (declare (type vector ar))
                         (print ar)
                         (array-has-fill-pointer-p ar)))))))
          '(values null &optional))))

(with-test (:name :--sign)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (x y)
                     (declare (integer x y))
                     (if (<= x y)
                         (- x y)
                         -10)))))
          '(values (integer * 0) &optional))))

(with-test (:name :--type)
  (assert
   (equal (third (sb-kernel:%simple-fun-type
                  (checked-compile
                   '(lambda (x y)
                     (if (> x y)
                         (- x y)
                         1)))))
          '(values real &optional))))

(with-test (:name :remove-equivalent-blocks-clear-constraints)
  (checked-compile-and-assert
      ()
      `(lambda (a c)
         (declare ((and fixnum unsigned-byte) a)
                  (fixnum c))
         (eql c
              (if (eql a c)
                  c
                  a)))
    ((3 1) nil)
    ((3 3) t)))

(with-test (:name :type-constraint-joining)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda ()
          (let ((x 'foo))
            (if (read)
                (setq x 3)
                (setq x 5))
            x)))))
    '(values (or (integer 5 5) (integer 3 3)) &optional))))

(with-test (:name :type-constraint-joining.2)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (etypecase x
            (integer (read))
            (float (read)))
          x))))
    '(values (or float integer) &optional))))

(with-test (:name :type-constraint-joining.3)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (if (read)
              (setq x (random 10))
              (setq x (random 10.0)))
          x))))
    '(values (or (single-float 0.0 (10.0)) (mod 10)) &optional))))

(with-test (:name :type-constraint-joining-terminates)
  (checked-compile
   sb-c::
   `(lambda (name vop)
      (block foo
        (do* ((block (vop-block vop) (ir2-block-prev block))
              (last vop (ir2-block-last-vop block)))
             (nil)
          (aver (eq (ir2-block-block block) (ir2-block-block (vop-block vop))))
          (do ((current last (vop-prev current)))
              ((null current))
            (when (eq (vop-name current) name)
              (return-from foo current))))))))

(with-test (:name :type-constraint-joining-conflicts)
  (assert (nth-value
           1
           (checked-compile
            '(lambda (y)
               (let ((x 'foo))
                 (ecase y
                   (1 (setq x (random 10)))
                   (2 (setq x (make-array 10)))
                   (3 (setq x (make-hash-table))))
                 (symbol-name x)))
            :allow-warnings t))))

(with-test (:name :type-constraint-joining.eql)
  (assert
   (equal (caddr
           (sb-kernel:%simple-fun-type
            (checked-compile
             `(lambda (x)
                (ecase x
                  (1 (read))
                  (2 (read)))
                x))))
          '(values (integer 1 2) &optional))))

(with-test (:name :type-constraint-joining.</=)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (declare (integer x))
                 (cond ((= x 20))
                       ((< x 5))
                       ((< x 10))
                       (t (error ""))) x))))
           '(values (or
                     (integer * 9)
                     (integer 20 20))
             &optional))))

(with-test (:name :type-constraint-joining.</=.2)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (if (typep x 'rational)
                     (cond ((= x 20))
                           ((< x 5))
                           ((< x 10))
                           (t (error "")))
                     (setf x :foo))
                 x))))
           '(values (or
                     (integer 20 20)
                     (rational * (10))
                     (member :foo))
             &optional))))

(with-test (:name :type-constraint-joining.</=.3)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (cond ((< x 5))
                       ((< x 10))
                       (t (error "")))
                 x))))
           '(values (or
                     (double-float * (10.0d0))
                     (single-float * (10.0))
                     (rational * (10)))
             &optional))))

(with-test (:name :type-constraint-joining.>/=)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x)
                 (declare (integer x))
                 (cond ((= x 5))
                       ((> x 20))
                       ((> x 10))
                       (t (error ""))) x))))
           '(values (or
                     (integer 11)
                     (integer 5 5))
             &optional))))

(with-test (:name :type-constraint-joining.complement)
  (assert
   (equal (caddr
           (sb-kernel:%simple-fun-type
            (checked-compile
             `(lambda (x)
                (if (read)
                    (cond ((typep x 'integer)
                           (error ""))
                          (t (print "")))
                    (cond ((typep x 'float)
                           (print ""))
                          (t (error ""))))
                x))))
          '(values (not integer) &optional))))

(with-test (:name (:type-constraint-joining :infinities 1))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (number)
                 (declare (type (or number null) number))
                 (cond ((null number) nil)
                       ((sb-ext:float-nan-p number) :nan)
                       ((= number sb-ext:double-float-positive-infinity) :inf)
                       ((= number sb-ext:double-float-negative-infinity) :-inf)
                       (number))))))
           '(values (or (member nil :nan :inf :-inf) float) &optional))))

(with-test (:name (:type-constraint-joining :infinities 2))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (number)
                 (declare (type (or number null) number))
                 (cond ((null number) nil)
                       ((sb-ext:float-nan-p number) :nan)
                       ((= number sb-ext:double-float-positive-infinity) number)
                       ((= number sb-ext:double-float-negative-infinity) number)
                       (t :number))))))
           `(values (or (single-float ,sb-ext:single-float-negative-infinity
                                      ,sb-ext:single-float-negative-infinity)
                        (double-float ,sb-ext:double-float-negative-infinity
                                      ,sb-ext:double-float-negative-infinity)
                        (single-float ,sb-ext:single-float-positive-infinity
                                      ,sb-ext:single-float-positive-infinity)
                        (double-float ,sb-ext:double-float-positive-infinity
                                      ,sb-ext:double-float-positive-infinity)
                        (member nil :nan :number))
                    &optional))))

(with-test (:name (:type-constraint-joining :infinities 3))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (number)
                 (declare (type (or number null) number))
                 (cond ((null number) nil)
                       ((sb-ext:float-nan-p number) :nan)
                       ((eql number sb-ext:double-float-positive-infinity) number)
                       ((eql number sb-ext:double-float-negative-infinity) number)
                       (t :number))))))
           `(values (or (double-float ,sb-ext:double-float-negative-infinity
                                      ,sb-ext:double-float-negative-infinity)
                        (double-float ,sb-ext:double-float-positive-infinity
                                      ,sb-ext:double-float-positive-infinity)
                        (member nil :nan :number))
                    &optional))))

(with-test (:name :debug-vars)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a)
                 (declare (optimize (debug 2)))
                 (let ((x (eq a 30)))
                   (if x
                       a
                       (error "")))))))
           `(values (integer 30 30) &optional))))

(with-test (:name :vector-length-constraints)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (declare (simple-vector x))
                 (when (< (length x) y)
                   (> (length x) y))))))
           `(values null &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (declare (vector x))
                 (when (< (length x) y)
                   (> (length x) y))))))
           `(values boolean &optional))))

(with-test (:name :non-commutative-invert)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (if (< x y)
                     (> y x)
                     (error ""))))))
            `(values (member t) &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (x y)
                 (if (< x y)
                     (< y x)
                     (error ""))))))
            `(values null &optional))))

(with-test (:name :=-real)
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a b)
                 (declare ((real 10 20) b))
                 (if (= a b)
                     a
                     (error ""))))))
           `(values (or (real 10 20) (complex (rational 10 20)) (complex (single-float 10.0 20.0))
                        (complex (double-float 10.0d0 20.0d0))) &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a b)
                 (declare (integer a)
                          ((real 10 20) b))
                 (if (= a b)
                     a
                     (error ""))))))
           `(values (integer 10 20) &optional)))
  (assert
   (ctype= (caddr
            (sb-kernel:%simple-fun-type
             (checked-compile
              `(lambda (a b)
                 (declare ((real 10 20) b))
                 (if (> a b)
                     a
                     (error ""))))))
           `(values (real (10)) &optional))))

(with-test (:name :<=-constraints)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (if (/= a 0)
             (if (>= a 0)
                 t)))
    ((1) t)
    ((0) nil)))

(with-test (:name :vector-length-derive-type)
  (let ((s "X"))
    (checked-compile-and-assert
        ()
        `(lambda (x)
           (declare ((or (vector t 2) (member #\@ ,s)) x))
           (typep x '(string 1)))
      ((s) t))))

(with-test (:name :constant-refs)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (let ((m 2301453501242805283))
           (if (< m a)
               (< m a)
               (error ""))))
    (((expt 2 80)) t)
    (:return-type (values (member t) &optional))))

(with-test (:name :constant-refs.2)
  (checked-compile-and-assert
      ()
      `(lambda (v)
         (if (<= -3483114449144072 v)
             0
             (if (>= -3483114449144072 v)
                 1
                 40)))
    ((9) 0)
    ((-3483114449144073) 1)
    (:return-type (values bit &optional))))

(with-test (:name :array-has-fill-pointer-p-constraint)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (s)
          (if (array-has-fill-pointer-p s)
              (error "")
              s)))))
    '(values array &optional))))

(with-test (:name :+integer-argument-constarint)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (a x y)
          (declare (fixnum x))
          (aref a (+ x y))
          y))))
    '(values integer &optional))))

(with-test (:name :+integer-argument-constarint.var)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (a x y)
          (declare (fixnum x))
          (let ((m (+ x y)))
            (aref a (+ m (aref a m)))
            y)))))
    '(values integer &optional))))

(with-test (:name :+integer-argument-constarint.typep)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (let ((m (+ 1 x)))
            (if (integerp m)
                (+ m x)
                2))))))
    '(values integer &optional))))

(with-test (:name :truncate-zero-remainder)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x y)
          (declare (integer x y))
          (multiple-value-bind (q r) (truncate x y)
            (if (zerop r)
                (error "~a" q)
                x))))))
    '(values (or (integer * -1) (integer 1)) &optional))))

(with-test (:name :vector-length-var)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (declare (simple-vector x))
          (if (< 3 (length x) 5)
              (length x)
              (error ""))))))
    '(values (integer 4 4) &optional))))

(with-test (:name :ash)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (declare ((integer 1) x))
          (let ((d (ash x -1)))
            (< d x))))))
    '(values (member t) &optional)))
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (declare ((integer 1) x))
          (let ((d (ash x -1)))
            (print d)
            (< d x))))))
    '(values (member t) &optional)))
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x y)
          (declare (unsigned-byte x)
                   ((integer * 0) y))
          (let ((d (ash x y)))
            (> d x))))))
    '(values null &optional))))

(with-test (:name :/)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x)
          (declare ((integer 1) x))
          (let ((d (truncate x 4)))
            (< d x))))))
    '(values (member t) &optional)))
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (x y)
          (declare ((integer 0) x y))
          (let ((d (/ x y)))
            (> d x))))))
    '(values null &optional))))

(with-test (:name :negate-<)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (a b)
          (declare (type integer a)
                   ((integer 2 3) b))
          (if (< (- a) b)
              a
              (loop))))))
    '(values (integer -2) &optional))))

(with-test (:name :eq-vector-lengths)
  (assert
   (type-specifiers-equal
    (caddr
     (sb-kernel:%simple-fun-type
      (checked-compile
       `(lambda (a b j)
          (declare (simple-vector a b)
                   ((integer 10 20) j)
                   (optimize (debug 1)))
          (when (and (= (length a) (length b))
                     (= j (length a)))
            (length b))))))
    '(values (or null (integer 10 20)) &optional))))

(defmacro assert-type (lambda type)
  `(assert
    (type-specifiers-equal
     (caddr
      (sb-kernel:%simple-fun-type
       (checked-compile
        ',lambda)))
     '(values ,type &optional))))

(with-test (:name :+>)
  (assert-type
   (lambda (a j)
     (declare (integer a)
              ((integer 1) j))
     (> (+ a j) a))
   (member t))
  (assert-type
   (lambda (a j)
     (declare (integer a)
              ((integer 0) j))
     (> (+ j a) a))
   boolean)
  (assert-type
   (lambda (a b j)
     (declare (integer a b)
              ((integer 1) j))
     (if (= b a)
         (let ((d (+ b j)))
           (> a d))
         (loop)))
   null)
  (assert-type
   (lambda (a b j)
     (declare (integer a b)
              ((integer 0) j))
     (if (<= a b)
         (let ((d (+ b j)))
           (>= a d))
         (loop)))
   boolean)
  (assert-type
   (lambda (a b j)
     (declare (integer a b)
              ((integer 1) j))
     (if (<= a b)
         (let ((d (+ b j)))
           (>= a d))
         (loop)))
   null))

(with-test (:name :->)
  (assert-type
   (lambda (v)
     (declare (integer v)
              (optimize (debug 2)))
     (let ((l (1- v)))
       (< l v)))
   (member t))
  (assert-type
   (lambda (v)
     (declare (simple-vector v)
              (optimize (debug 2)))
     (let ((l (1- (length v))))
       (< l (length v))))
   (member t))
  (assert-type
   (lambda (v)
     (declare (integer v)
              (optimize (debug 1)))
     (let ((l (1- v)))
       (< l v)))
   (member t))
  (assert-type
   (lambda (v)
     (declare (simple-vector v)
              (optimize (debug 1)))
     (let ((l (1- (length v))))
       (< l (length v))))
   (member t)))
