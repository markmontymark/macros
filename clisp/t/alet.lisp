(load "../let-over-lambda.lisp")

(lisp-unit:define-test alet-test

	(setf (symbol-function 'aletter)
		(alet ((acc 0))
				(alambda (n)
					(if (eq n 'invert)
						(setq this
									(lambda (n)
										(if (eq n 'invert)
											(setq this #'self)
											(decf acc n))))
						(incf acc n)))))

	 (lisp-unit:assert-equal (aletter 10) 10)
	 (lisp-unit:assert-equal (aletter 10) 20)

	 (aletter 'invert)
	 (lisp-unit:assert-equal (aletter 3) 17)

	 (aletter 'invert)
	 (lisp-unit:assert-equal (aletter 5) 22)

)


(lisp-unit:write-tap (lisp-unit:run-tests :all))
