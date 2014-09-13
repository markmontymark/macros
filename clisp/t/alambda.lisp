(load "../let-over-lambda.lisp")

(lisp-unit:define-test alambda-test

 ; Factorial function defined recursively where `self' refers to the alambda function
	 (let 
		((n 10)
		 (factorial (alambda (n) 
								(if (= n 0)
									1 
									(* n (self (1- n)))))))

	 (lisp-unit:assert-equal
		 (funcall factorial 2) 2)

	 (lisp-unit:assert-equal
		 (funcall factorial 3) 6)

	 (lisp-unit:assert-equal
		 (funcall factorial 4) 24)

	 (lisp-unit:assert-equal
		 (funcall factorial 3) 6)

	 ;; test unwanted variable capture
	 (lisp-unit:assert-equal
		 n 10)))


(lisp-unit:write-tap (lisp-unit:run-tests :all))
