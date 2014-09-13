(load "../let-over-lambda.lisp")

(lisp-unit:define-test dlambda-test

(setf (symbol-function 'count-test)
		 (let ((count 0))
			 (dlambda
				 (:inc () (incf count))
				 (:dec () (decf count)))))
; Factorial function defined recursively where `self' refers to the alambda function
(let 
 ((a (count-test :inc))
  (b (count-test :inc))
  (c (count-test :dec))
  (d (count-test :dec)))

 (lisp-unit:assert-equal a 1)
 (lisp-unit:assert-equal b 2)
 (lisp-unit:assert-equal c 1)
 (lisp-unit:assert-equal d 0)))

(lisp-unit:write-tap (lisp-unit:run-tests :all))
