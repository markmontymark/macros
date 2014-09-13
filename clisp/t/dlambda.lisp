(load "../let-over-lambda.lisp")

(lisp-unit:define-test dlambda-test

(setf (symbol-function 'count-test)
		 (let ((count 0))
			 (dlambda
				 (:inc () (incf count))
				 (:dec () (decf count)))))

(setf (symbol-function 'bigger-count-test)
    (let ((count 0))
      (dlambda
        (:value () count)
        (:reset () (setf count 0))
        (:inc (n) (incf count n))
        (:dec (n) (decf count n))
        (:bound (lo hi)
          (setf count
            (min hi
                 (max lo
                      count)))))))
(let 
 ((a (count-test :inc))
  (b (count-test :inc))
  (c (count-test :dec))
  (d (count-test :dec))
  (e (bigger-count-test :inc 5))
  (f (bigger-count-test :dec 3)))

 ; count-test
 (lisp-unit:assert-equal a 1)
 (lisp-unit:assert-equal b 2)
 (lisp-unit:assert-equal c 1)
 (lisp-unit:assert-equal d 0)

 ; bigger-count-test
 (lisp-unit:assert-equal e 5)
 (lisp-unit:assert-equal f 2)
 
 (bigger-count-test :reset)

 (lisp-unit:assert-equal (bigger-count-test :value) 0)

 (bigger-count-test :bound 1 3)

 (lisp-unit:assert-equal (bigger-count-test :value) 1)
 (bigger-count-test :inc 5)
 (bigger-count-test :bound 1 3)
 (lisp-unit:assert-equal (bigger-count-test :value) 3)))

(lisp-unit:write-tap (lisp-unit:run-tests :all))
