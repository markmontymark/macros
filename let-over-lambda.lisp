;; Antiweb (C) Doug Hoyte

;; This is a "production" version of LOL with bug-fixes
;; and new features in the spirit of the book.

;; See http://letoverlambda.com

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!





(defun mkstr (&rest args)
	(with-output-to-string (s)
		(dolist (a args) (princ a s))))

(defun symb (&rest args)
	(values (intern (apply #'mkstr args))))



(defun group (source n)
	(if (zerop n) (error "zero length"))
	(labels ((rec (source acc)
								(let ((rest (nthcdr n source)))
									(if (consp rest)
										(rec rest (cons
																(subseq source 0 n)
																acc))
										(nreverse
											(cons source acc))))))
		(if source (rec source nil) nil)))



(defun flatten (x)
	(labels ((rec (x acc)
								(cond ((null x) acc)
											((atom x) (cons x acc))
											(t (rec
													 (car x)
													 (rec (cdr x) acc))))))
		(rec x nil)))



(defun fact (x)
	(if (= x 0)
		1
		(* x (fact (- x 1)))))

(defun choose (n r)
	(/ (fact n)
		 (fact (- n r))
		 (fact r)))




(defun g!-symbol-p (s)
	(and (symbolp s)
			 (> (length (symbol-name s)) 2)
			 (string= (symbol-name s)
								"G!"
								:start1 0
								:end1 2)))



(defmacro defmacro/g! (name args &rest body)
	(let ((syms (remove-duplicates
								(remove-if-not #'g!-symbol-p
															 (flatten body)))))
		`(defmacro ,name ,args
			 (let ,(mapcar
							 (lambda (s)
								 `(,s (gensym ,(subseq
																 (symbol-name s)
																 2))))
							 syms)
				 ,@body))))



(defun o!-symbol-p (s)
	(and (symbolp s)
			 (> (length (symbol-name s)) 2)
			 (string= (symbol-name s)
								"O!"
								:start1 0
								:end1 2)))

(defun o!-symbol-to-g!-symbol (s)
	(symb "G!"
				(subseq (symbol-name s) 2)))



(defmacro defmacro! (name args &rest body)
	(let* ((os (remove-if-not #'o!-symbol-p args))
				 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
		`(defmacro/g! ,name ,args
									`(let ,(mapcar #'list (list ,@gs) (list ,@os))
										 ,(progn ,@body)))))




;; Nestable suggestion from Daniel Herring

(defun |#"-reader| (stream sub-char numarg)
	(declare (ignore sub-char numarg))
	(let (chars (state 'normal) (depth 1))
		(loop do
					(let ((curr (read-char stream)))
						(cond ((eq state 'normal)
									 (cond ((char= curr #\#)
													(push #\# chars)
													(setq state 'read-sharp))
												 ((char= curr #\")
													(setq state 'read-quote))
												 (t
													 (push curr chars))))
									((eq state 'read-sharp)
									 (cond ((char= curr #\")
													(push #\" chars)
													(incf depth)
													(setq state 'normal))
												 (t
													 (push curr chars)
													 (setq state 'normal))))
									((eq state 'read-quote)
									 (cond ((char= curr #\#)
													(decf depth)
													(if (zerop depth) (return))
													(push #\" chars)
													(push #\# chars)
													(setq state 'normal))
												 (t
													 (push #\" chars)
													 (if (char= curr #\")
														 (setq state 'read-quote)
														 (progn
															 (push curr chars)
															 (setq state 'normal)))))))))
		(coerce (nreverse chars) 'string)))

(set-dispatch-macro-character
	#\# #\" #'|#"-reader|)





; This version is from Martin Dirichs

(defun |#>-reader| (stream sub-char numarg)
	(declare (ignore sub-char numarg))
	(let (chars)
		(do ((curr (read-char stream)
							 (read-char stream)))
			((char= #\newline curr))
			(push curr chars))
		(let ((pattern (nreverse chars))
					output)
			(labels ((match (pos chars)
											(if (null chars)
												pos
												(if (char= (nth pos pattern) (car chars))
													(match (1+ pos) (cdr chars))
													(match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
				(do (curr
							(pos 0))
					((= pos (length pattern)))
					(setf curr (read-char stream)
								pos (match pos (list curr)))
					(push curr output))
				(coerce
					(nreverse
						(nthcdr (length pattern) output))
					'string)))))


(set-dispatch-macro-character
	#\# #\> #'|#>-reader|)



(defun segment-reader (stream ch n)
	(if (> n 0)
		(let ((chars))
			(do ((curr (read-char stream)
								 (read-char stream)))
				((char= ch curr))
				(push curr chars))
			(cons (coerce (nreverse chars) 'string)
						(segment-reader stream ch (- n 1))))))



#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
					 ``(lambda (,',g!str)
							 (cl-ppcre:scan
								 ,(if (zerop (length ,g!mods))
										(car ,g!args)
										(format nil "(?~a)~a" ,g!mods (car ,g!args)))
								 ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
					 ``(lambda (,',g!str)
							 (cl-ppcre:regex-replace-all
								 ,(car ,g!args)
								 ,',g!str
								 ,(cadr ,g!args))))



#+cl-ppcre
(defun |#~-reader| (stream sub-char numarg)
	(declare (ignore sub-char numarg))
	(let ((mode-char (read-char stream)))
		(cond
			((char= mode-char #\m)
			 (match-mode-ppcre-lambda-form
				 (segment-reader stream
												 (read-char stream)
												 1)
				 (coerce (loop for c = (read-char stream)
											 while (alpha-char-p c)
											 collect c
											 finally (unread-char c stream))
								 'string)))
			((char= mode-char #\s)
			 (subst-mode-ppcre-lambda-form
				 (segment-reader stream
												 (read-char stream)
												 2)))
			(t (error "Unknown #~~ mode character")))))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ #'|#~-reader|)







(defmacro! dlambda (&rest ds)
					 `(lambda (&rest ,g!args)
							(case (car ,g!args)
								,@(mapcar
										(lambda (d)
											`(,(if (eq t (car d))
													 t
													 (list (car d)))
												 (apply (lambda ,@(cdr d))
																,(if (eq t (car d))
																	 g!args
																	 `(cdr ,g!args)))))
										ds))))



;; Graham's alambda
(defmacro alambda (parms &body body)
	`(labels ((self ,parms ,@body))
		 #'self))



;; Graham's aif
(defmacro aif (test then &optional else)
	`(let ((it ,test))
		 (if it ,then ,else)))



(defun |#`-reader| (stream sub-char numarg)
	(declare (ignore sub-char))
	(unless numarg (setq numarg 1))
	`(lambda ,(loop for i from 1 to numarg
									collect (symb 'a i))
		 ,(funcall
				(get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
	#\# #\` #'|#`-reader|)



(defmacro alet% (letargs &rest body)
	`(let ((this) ,@letargs)
		 (setq this ,@(last body))
		 ,@(butlast body)
		 this))



(defmacro alet (letargs &rest body)
	`(let ((this) ,@letargs)
		 (setq this ,@(last body))
		 ,@(butlast body)
		 (lambda (&rest params)
			 (apply this params))))



(defun let-binding-transform (bs)
	(if bs
		(cons
			(cond ((symbolp (car bs))
						 (list (car bs)))
						((consp (car bs))
						 (car bs))
						(t
							(error "Bad let bindings")))
			(let-binding-transform (cdr bs)))))


(defmacro pandoriclet (letargs &rest body)
	(let ((letargs (cons
									 '(this)
									 (let-binding-transform
										 letargs))))
		`(let (,@letargs)
			 (setq this ,@(last body))
			 ,@(butlast body)
			 (dlambda
				 (:pandoric-get (sym)
												,(pandoriclet-get letargs))
				 (:pandoric-set (sym val)
												,(pandoriclet-set letargs))
				 (t (&rest args)
						(apply this args))))))



(defun pandoriclet-get (letargs)
	`(case sym
		 ,@(mapcar #`((,(car a1)) ,(car a1))
							 letargs)
		 (t (error
					"Unknown pandoric get: ~a"
					sym))))

(defun pandoriclet-set (letargs)
	`(case sym
		 ,@(mapcar #`((,(car a1))
									(setq ,(car a1) val))
							 letargs)
		 (t (error
					"Unknown pandoric set: ~a"
					sym))))



(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
	(funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
				 `(progn
						(funcall ,box :pandoric-set ,sym ,val)
						,val))



(defmacro with-pandoric (syms box &rest body)
	(let ((g!box (gensym "box")))
		`(let ((,g!box ,box))
			 (declare (ignorable ,g!box))
			 (symbol-macrolet
				 (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
										syms))
				 ,@body))))



(defun pandoric-hotpatch (box new)
	(with-pandoric (this) box
								 (setq this new)))



(defmacro pandoric-recode (vars box new)
	`(with-pandoric (this ,@vars) ,box
									(setq this ,new)))



(defmacro plambda (largs pargs &rest body)
	(let ((pargs (mapcar #'list pargs)))
		`(let (this self)
			 (setq
				 this (lambda ,largs ,@body)
				 self (dlambda
								(:pandoric-get (sym)
															 ,(pandoriclet-get pargs))
								(:pandoric-set (sym val)
															 ,(pandoriclet-set pargs))
								(t (&rest args)
									 (apply this args)))))))



(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
	`(let ((pandoric-eval-tunnel
					 (plambda () ,vars t)))
		 (eval `(with-pandoric
							,',vars pandoric-eval-tunnel
							,,expr))))


;; Chapter 7

(set-dispatch-macro-character #\# #\f
															(lambda (stream sub-char numarg)
																(declare (ignore stream sub-char))
																(setq numarg (or numarg 3))
																(unless (<= numarg 3)
																	(error "Bad value for #f: ~a" numarg))
																`(declare (optimize (speed ,numarg)
																										(safety ,(- 3 numarg))))))



(defmacro fast-progn (&rest body)
	`(locally #f ,@body))



(defmacro safe-progn (&rest body)
	`(locally #0f ,@body))





(defun fformat (&rest all)
	(apply #'format all))

(define-compiler-macro fformat
											 (&whole form
															 stream fmt &rest args)
											 (if (constantp fmt)
												 (if stream
													 `(funcall (formatter ,fmt)
																		 ,stream ,@args)
													 (let ((g!stream (gensym "stream")))
														 `(with-output-to-string (,g!stream)
																(funcall (formatter ,fmt)
																				 ,g!stream ,@args))))
												 form))



(declaim (inline make-tlist tlist-left
								 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))



(declaim (inline tlist-add-left
								 tlist-add-right))

(defun tlist-add-left (tl it)
	(let ((x (cons it (car tl))))
		(if (tlist-empty-p tl)
			(setf (cdr tl) x))
		(setf (car tl) x)))

(defun tlist-add-right (tl it)
	(let ((x (cons it nil)))
		(if (tlist-empty-p tl)
			(setf (car tl) x)
			(setf (cddr tl) x))
		(setf (cdr tl) x)))



(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
	(if (tlist-empty-p tl)
		(error "Remove from empty tlist")
		(let ((x (car tl)))
			(setf (car tl) (cdar tl))
			(if (tlist-empty-p tl)
				(setf (cdr tl) nil)) ;; For gc
			(car x))))



(declaim (inline tlist-update))

(defun tlist-update (tl)
	(setf (cdr tl) (last (car tl))))



(defun build-batcher-sn (n)
	(let* (network
					(tee (ceiling (log n 2)))
					(p (ash 1 (- tee 1))))
		(loop while (> p 0) do
					(let ((q (ash 1 (- tee 1)))
								(r 0)
								(d p))
						(loop while (> d 0) do
									(loop for i from 0 to (- n d 1) do
												(if (= (logand i p) r)
													(push (list i (+ i d))
																network)))
									(setf d (- q p)
												q (ash q -1)
												r p)))
					(setf p (ash p -1)))
		(nreverse network)))



(defmacro! sortf (comparator &rest places)
					 (if places
						 `(tagbody
								,@(mapcar
										#`(let ((,g!a #1=,(nth (car a1) places))
														(,g!b #2=,(nth (cadr a1) places)))
												(if (,comparator ,g!b ,g!a)
													(setf #1# ,g!b
																#2# ,g!a)))
										(build-batcher-sn (length places))))))






;;;;;; NEW CODE FOR ANTIWEB



#+cl-ppcre
(defun dollar-symbol-p (s)
	(and (symbolp s)
			 (> (length (symbol-name s)) 1)
			 (string= (symbol-name s)
								"$"
								:start1 0
								:end1 1)
			 (ignore-errors (parse-integer (subseq (symbol-name s) 1)))))



(defun prune-if-match-bodies-from-sub-lexical-scope (tree)
	(if (consp tree)
		(if (or (eq (car tree) 'if-match)
						(eq (car tree) 'when-match))
			(cddr tree)
			(cons (prune-if-match-bodies-from-sub-lexical-scope (car tree))
						(prune-if-match-bodies-from-sub-lexical-scope (cdr tree))))
		tree))

;; WARNING: Not %100 correct. Removes forms like (... if-match ...) from the
;; sub-lexical scope even though this isn't an invocation of the macro.
#+cl-ppcre
(defmacro! if-match ((test str) conseq &optional altern)
					 (let ((dollars (remove-duplicates
														(remove-if-not #'dollar-symbol-p
																					 (flatten (prune-if-match-bodies-from-sub-lexical-scope conseq))))))
						 (let ((top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>)) 0)))
							 `(let ((,g!str ,str))
									(multiple-value-bind (,g!s ,g!e ,g!ms ,g!me) (,test ,g!str)
										(declare (ignorable ,g!e ,g!me))
										(if ,g!s
											(if (< (length ,g!ms) ,top)
												(error "ifmatch: too few matches")
												(let ,(mapcar #`(,(symb "$" a1) (subseq ,g!str (aref ,g!ms ,(1- a1))
																																(aref ,g!me ,(1- a1))))
																			(loop for i from 1 to top collect i))
													,conseq))
											,altern))))))

(defmacro when-match ((test str) conseq &rest more-conseq)
	`(if-match (,test ,str)
						 (progn ,conseq ,@more-conseq)))





(defvar forth-registers
	'(pstack rstack pc
					 dict compiling dtable))



(defstruct forth-word
	name prev immediate thread)



(defun forth-lookup (w last)
	(if last
		(if (eql (forth-word-name last) w)
			last
			(forth-lookup
				w (forth-word-prev last)))))



(defmacro forth-inner-interpreter ()
	`(loop
		 do (cond
					((functionp (car pc))
					 (funcall (car pc)))
					((consp (car pc))
					 (push (cdr pc) rstack)
					 (setf pc (car pc)))
					((null pc)
					 (setf pc (pop rstack)))
					(t
						(push (car pc) pstack)
						(setf pc (cdr pc))))
		 until (and (null pc) (null rstack))))



;; Prim-form: (name immediate . forms)
(defvar forth-prim-forms nil)

(defmacro def-forth-naked-prim (&rest code)
	`(push ',code forth-prim-forms))

(defmacro def-forth-prim (&rest code)
	`(def-forth-naked-prim
		 ,@code
		 (setf pc (cdr pc))))


(def-forth-prim nop nil)

(def-forth-prim * nil
								(push (* (pop pstack) (pop pstack))
											pstack))

(def-forth-prim drop nil
								(pop pstack))

(def-forth-prim dup nil
								(push (car pstack) pstack))

(def-forth-prim swap nil
								(rotatef (car pstack) (cadr pstack)))

(def-forth-prim print nil
								(print (pop pstack)))

(def-forth-prim >r nil
								(push (pop pstack) rstack))

(def-forth-prim r> nil
								(push (pop rstack) pstack))



(defmacro! go-forth (o!forth &rest words)
	 `(dolist (w ',words)
			(funcall ,g!forth w)))



(defvar forth-stdlib nil)

(defmacro forth-stdlib-add (&rest all)
	`(setf forth-stdlib
				 (nconc forth-stdlib
								',all)))



(defmacro new-forth ()
	`(alet ,forth-registers
				 (setq dtable (make-hash-table))
				 (forth-install-prims)
				 (dolist (v forth-stdlib)
					 (funcall this v))
				 (plambda (v) ,forth-registers
									(let ((word (forth-lookup v dict))) 
										(if word
											(forth-handle-found)
											(forth-handle-not-found))))))



;; Prim-form: (name immediate . forms)
(defmacro forth-install-prims ()
	`(progn
		 ,@(mapcar
				 #`(let ((thread (lambda ()
													 ,@(cddr a1))))
						 (setf dict
									 (make-forth-word
										 :name ',(car a1)
										 :prev dict
										 :immediate ,(cadr a1)
										 :thread thread))
						 (setf (gethash thread dtable)
									 ',(cddr a1)))
				 forth-prim-forms)))



(def-forth-prim [ t ; <- t means immediate
									(setf compiling nil))

									(def-forth-prim ] nil ; <- not immediate
									(setf compiling t))



(defmacro forth-compile-in (v)
	`(setf (forth-word-thread dict)
				 (nconc (forth-word-thread dict)
								(list ,v))))



(defmacro forth-handle-found ()
	`(if (and compiling
						(not (forth-word-immediate word)))
		 (forth-compile-in (forth-word-thread word))
		 (progn
			 (setf pc (list (forth-word-thread word)))
			 (forth-inner-interpreter))))



(defmacro forth-handle-not-found ()
	`(cond
		 ((and (consp v) (eq (car v) 'quote))
			(if compiling
				(forth-compile-in (cadr v))
				(push (cadr v) pstack)))
		 ((and (consp v) (eq (car v) 'postpone))
			(let ((word (forth-lookup (cadr v) dict)))
				(if (not word)
					(error "Postpone failed: ~a" (cadr v)))
				(forth-compile-in (forth-word-thread word))))
		 ((symbolp v)
			(error "Word ~a not found" v))
		 (t
			 (if compiling
				 (forth-compile-in v)
				 (push v pstack)))))



(def-forth-prim create nil
								(setf dict (make-forth-word :prev dict)))

(def-forth-prim name nil
								(setf (forth-word-name dict) (pop pstack)))

(def-forth-prim immediate nil
								(setf (forth-word-immediate dict) t))



(forth-stdlib-add
	create
	] create ] [
		'{ name)


(forth-stdlib-add
	{ (postpone [) [
		'} name immediate)



(def-forth-prim @ nil
	(push (car (pop pstack))
				pstack))

(def-forth-prim ! nil
	(let ((location (pop pstack)))
		(setf (car location) (pop pstack))))



(defmacro forth-unary-word-definer (&rest words)
	`(progn
		 ,@(mapcar
				 #`(def-forth-prim ,a1 nil
													 (push (,a1 (pop pstack))
																 pstack))
				 words)))



(defmacro! forth-binary-word-definer (&rest words)
	 `(progn
			,@(mapcar
					#`(def-forth-prim ,a1 nil
														(let ((,g!top (pop pstack)))
															(push (,a1 (pop pstack)
																				 ,g!top)
																		pstack)))
				words)))


(forth-unary-word-definer
	not car cdr cadr caddr cadddr
	oddp evenp)
(forth-binary-word-definer
	eq equal + - / = < > <= >=
	max min and or)



(def-forth-naked-prim branch-if nil
	(setf pc (if (pop pstack)
						 (cadr pc)
						 (cddr pc))))



(forth-stdlib-add
	{ r> drop } 'exit name)



(def-forth-naked-prim compile nil
	(setf (forth-word-thread dict)
				(nconc (forth-word-thread dict)
							 (list (cadr pc))))
	(setf pc (cddr pc)))

(def-forth-prim here nil
	(push (last (forth-word-thread dict))
				pstack))



(forth-stdlib-add
	{ compile not
	compile branch-if
	compile nop
	here } 'if name immediate)



(forth-stdlib-add
	{ compile nop
	here swap ! } 'then name immediate)



(forth-stdlib-add
	{ 0 swap - } 'negate name
	{ dup 0 < if negate then } 'abs name)



(forth-stdlib-add
	{ compile 't
	compile branch-if
	compile nop
	here swap
	compile nop
	here swap ! } 'else name immediate)



(forth-stdlib-add
	{ evenp if 0 else 1 then } 'mod2 name)



(forth-stdlib-add
	{ compile nop
	here } 'begin name immediate
	{ compile 't
	compile branch-if
	compile nop
	here ! } 'again name immediate)



(defun get-forth-thread (forth word)
	(with-pandoric (dict) forth
								 (forth-word-thread
									 (forth-lookup word dict))))

(defun print-forth-thread (forth word)
	(let ((*print-circle* t))
		(print (get-forth-thread forth word))
		t))



(defmacro flubify-aux ()
	`(alambda (c)
						(if c
							(cond
								((gethash (car c) prim-ht)
								 (assemble-flub
									 `(funcall
											,(gethash (car c) prim-ht))
									 (self (cdr c))))
								((gethash (car c) thread-ht)
								 (assemble-flub
									 `(funcall #',(car (gethash (car c)
																							thread-ht)))
									 (self (cdr c))))
								((eq (car c) branch-if)
								 (assemble-flub
									 `(if (pop pstack)
											(go ,(gethash (cadr c) go-ht)))
									 (self (cddr c))))
								((consp (car c))
								 (flubify forth (car c) prim-ht
													thread-ht branch-if)
								 (self c))
								(t
									(assemble-flub
										`(push ',(car c) pstack)
										(self (cdr c))))))))



(defmacro assemble-flub (form rest)
	`(if (gethash c go-ht)
		 (list* (gethash c go-ht)
						,form
						,rest)
		 (list* ,form
						,rest)))



(defun flubify (forth thread prim-ht
											thread-ht branch-if)
	(unless #1=(gethash thread thread-ht)
		(setf #1# (list (gensym)))
		(let ((go-ht (make-hash-table)))
			(funcall
				(alambda (c)
								 (when c
									 (cond
										 ((eq (car c) branch-if)
											(setf (gethash (cadr c) go-ht)
														(gensym))
											(self (cddr c)))
										 ((consp (car c))
											(flubify forth thread prim-ht
															 thread-ht branch-if)))
									 (self (cdr c))))
				thread)
			(setf #1# (nconc #1# (funcall
														 (flubify-aux)
														 thread))))))



(defun compile-flubified (thread thread-ht)
	`(labels (,@(let (collect)
								(maphash
									(lambda (k v)
										(declare (ignore k))
										(push
											`(,(car v) ()
																 (tagbody ,@(cdr v)))
											collect))
									thread-ht)
								(nreverse collect)))
		 (funcall #',(car (gethash thread thread-ht)))))



(defun flubify-thread-shaker
	(forth thread ht tmp-ht branch-if compile)
	(if (gethash thread tmp-ht)
		(return-from flubify-thread-shaker)
		(setf (gethash thread tmp-ht) t))
	(cond
		((and (consp thread) (eq (car thread) branch-if))
		 (if (cddr thread)
			 (flubify-thread-shaker
				 forth (cddr thread) ht
				 tmp-ht branch-if compile)))
		((and (consp thread) (eq (car thread) compile))
		 (error "Can't convert compiling word to lisp"))
		((consp thread)
		 (flubify-thread-shaker
			 forth (car thread) ht
			 tmp-ht branch-if compile)
		 (if (cdr thread)
			 (flubify-thread-shaker
				 forth (cdr thread) ht
				 tmp-ht branch-if compile)))
		((not (gethash thread ht))
		 (if (functionp thread)
			 (setf (gethash thread ht)
						 (with-pandoric (dtable) forth
														(gethash thread dtable)))))))



(defun forth-to-lisp (forth word)
	(let ((thread (get-forth-thread forth word))
				(shaker-ht (make-hash-table))
				(prim-ht (make-hash-table))
				(thread-ht (make-hash-table))
				(branch-if (get-forth-thread forth 'branch-if))
				(compile (get-forth-thread forth 'compile)))
		(flubify-thread-shaker
			forth thread shaker-ht
			(make-hash-table) branch-if compile)
		(maphash (lambda (k v)
							 (declare (ignore v))
							 (setf (gethash k prim-ht) (gensym)))
						 shaker-ht)
		(flubify forth thread prim-ht thread-ht branch-if)
		`(let (pstack)
			 (let (,@(let (collect)
								 (maphash
									 (lambda (k v)
										 (push `(,(gethash k prim-ht)
															(lambda () ,@(butlast v)))
													 collect))
									 shaker-ht)
								 (nreverse collect)))
				 ,(compile-flubified
						thread thread-ht)))))

