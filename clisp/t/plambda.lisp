(load "../let-over-lambda.lisp")

(lisp-unit:define-test plambda-test

(setf (symbol-function 'pantest)
		 (let ((a 0))
			 (let ((b 1))
				 (plambda (n) (a b)
									(incf a n)
									(setq b (* b n))))))

(defun pantest-peek ()
	(with-pandoric (a b) #'pantest
		(format nil "a=~a, b=~a" a b)))

(let 
	()
	 (lisp-unit:assert-equal (pantest-peek) "a=0, b=1"))


(defmacro defpan (name args &rest body)
  `(defun ,name (self)
     ,(if args
        `(with-pandoric ,args self
           ,@body)
        `(progn ,@body))))

(defpan stats-counter-mean (sum count)
  (/ sum count))

(defpan stats-counter-variance
        (sum-of-squares sum count)
  (if (< count 2)
    0
    (/ (- sum-of-squares
          (* sum
             (stats-counter-mean self)))
       (- count 1))))

(defpan stats-counter-stddev ()
  (sqrt (stats-counter-variance self)))

(defun make-noisy-stats-counter
       (&key (count 0)
             (sum 0)
             (sum-of-squares 0))
  (plambda (n) (sum count sum-of-squares)
    (incf sum-of-squares (expt n 2))
    (incf sum n)
    (incf count)

    (format nil 
      "MEAN=~a VAR=~a STDDEV=~a"
           (stats-counter-mean self)
           (stats-counter-variance self)
           (stats-counter-stddev self))))

(let
	((my-counter (make-noisy-stats-counter)))
	(lisp-unit:assert-equal (funcall my-counter 3) "MEAN=3 VAR=0 STDDEV=0")
	(lisp-unit:assert-equal (funcall my-counter 4) "MEAN=7/2 VAR=1/2 STDDEV=0.70710677")
	(lisp-unit:assert-equal (funcall my-counter 5) "MEAN=4 VAR=1 STDDEV=1")
	(lisp-unit:assert-equal (funcall my-counter 3) "MEAN=15/4 VAR=11/12 STDDEV=0.95742714")))





(lisp-unit:write-tap (lisp-unit:run-tests :all))
