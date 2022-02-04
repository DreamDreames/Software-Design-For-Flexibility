(define (compose f g)
 (define (the-compose . args)
 	(let ((m (get-arity f))
 	      (n (get-arity g)))
	  (assert (= m n))
	  (assert (= n (length args)))
	  (restrict-arity the-compose n))
	(f (apply g args)))
 the-compose)

(write-line
 ((compose 
 	(lambda (x) (list 'foo x))
	(lambda (x) (list 'bar x)))
 'z))

(define (parallel-combine h f g)
 (define (the-parallel-combine . args)
 	(let ((x (get-arity f))
	      (y (get-arity g))
	      (z (procedure-arity-max (procedure-arity h))))
	 (assert (= x y))
	 (assert (= x (length args)))
	 (assert (or (eqv? z #f) (>= z 2)))
	 ;;(assert (eqv? z 2))
	 (restrict-arity the-parallel-combine x)
	 (h (apply f args) (apply g args))))
 the-parallel-combine)

(write-line
 ;((parallel-combine cons
 ((parallel-combine list
 	(lambda (x y z) (list 'foo x y z))
	(lambda (u v w) (list 'bar u v w)))
  'a 'b 'c))

(define (get-arity proc)
 (or (hash-table-ref/default arity-table proc #f)
 	(let ((a (procedure-arity proc)))
	 (assert (eqv? (procedure-arity-min a)
	 		(procedure-arity-max a)))
	(procedure-arity-min a))))

(define (restrict-arity proc nargs)
 (hash-table-set! arity-table proc nargs)
 proc)

(define arity-table (make-key-weak-eqv-hash-table))