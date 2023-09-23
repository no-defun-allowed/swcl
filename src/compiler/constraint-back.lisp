;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun constraint-propagate-back (lvar kind constraint gen consequent alternative)
  (multiple-value-bind (node nth-value) (mv-principal-lvar-ref-use lvar)
    (when (combination-p node)
      (binding* ((info (combination-fun-info node) :exit-if-null)
                 (propagate (fun-info-constraint-propagate-back info)
                            :exit-if-null))
        (funcall propagate node nth-value kind constraint gen consequent alternative)))))

(defoptimizer (+ constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     ;; (integerp (+ integer y)) means Y is an integer too.
     (flet ((add (lvar)
              (let ((var (ok-lvar-lambda-var lvar gen)))
                (when var
                  (conset-add-constraint-to-eql consequent 'typep var (specifier-type 'integer) nil)))))
       (when (csubtypep constraint (specifier-type 'integer))
         (let ((x-integerp (csubtypep (lvar-type x) (specifier-type 'integer)))
               (y-integerp (csubtypep (lvar-type y) (specifier-type 'integer))))
           (cond ((and x-integerp
                       (not y-integerp))
                  (add y))
                 ((and y-integerp
                       (not x-integerp))
                  (add x)))))))))

;;; If the remainder is non-zero then X can't be zero.
(defoptimizer (truncate constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (let ((var (ok-lvar-lambda-var x gen)))
   (when (and var
              (eql nth-value 1)
              (csubtypep (lvar-type x) (specifier-type 'integer))
              (csubtypep (lvar-type y) (specifier-type 'integer)))
     (case kind
       (eql
        (when (and (constant-p constraint)
                   (eql (constant-value constraint) 0)
                   alternative)
          (conset-add-constraint-to-eql alternative 'typep var (specifier-type '(and integer (not (eql 0)))) nil)))
       (>
        (when (csubtypep (lvar-type constraint) (specifier-type '(integer 0)))
          (conset-add-constraint-to-eql consequent 'typep var (specifier-type '(integer 1)) nil)))))))

(defoptimizer (%negate constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (<
     (when (and (csubtypep (lvar-type x) (specifier-type 'rational))
                (csubtypep (lvar-type constraint) (specifier-type 'rational)))
       (let ((range (type-approximate-interval (lvar-type constraint))))
         (when (and range
                    (numberp (interval-high range)))
           (let ((var (ok-lvar-lambda-var x gen)))
             (when var
               (conset-add-constraint-to-eql consequent 'typep var (specifier-type `(rational (,(- (interval-high range)))))
                                             nil)))))))))
