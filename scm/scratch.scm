;; Trivial

(define null '())

;; HOF
(define (reduce op nil seq)
  (if (null? seq)
      nil
      (op (car seq)
          (reduce op nil (cdr seq)))))

(define (fold op nil seq)
  (let recur ((acc nil)
              (seq seq))
    (if (null? seq)
        acc
        (recur (op acc (car seq))
               (cdr seq)))))

(define (any pred seq)
  (fold (lambda (acc x)
          (or acc (pred x)))
        #f seq))

(define (every pred seq)
  (fold (lambda (acc x)
          (and acc (pred x)))
        #t seq))

;; Cantrips

(define (identity x) x)

(define (always x)
  (lambda args x))

(define (project n)
  (lambda args
    (list-ref args n)))

(define (complement pred)
  (lambda args
    (not (apply pred args))))

(define (disjoin . preds)
  (lambda args
    (fold (lambda (acc pred)
            (or acc (apply pred args)))
          #f preds)))

(define (conjoin . preds)
  (lambda args
    (fold (lambda (acc pred)
            (and acc (apply pred args)))
          #t preds)))

(define (compose . procs)
  (lambda (val)
    (reduce (lambda (proc acc)
              (proc acc))
            val procs)))

(define true (always #t))

(define false (always #f))

(define (take seq n)
  (if (or (zero? n) (null? seq))
      null
      (cons (car seq)
            (take (cdr seq) (- n 1)))))

(define (drop seq n)
  (if (or (zero? n) (null? seq))
      seq
      (drop (cdr seq) (- n 1))))

;; Macrology

(define-syntax ignore
  (syntax-rules ()
    ((ignore forms ...)
     (void))))

(ignore
  (put 'ignore 'scheme-indent-function 0))

(define-syntax pipe
  (syntax-rules ()
    ((pipe in)
     in)
    ((pipe in (f args ...) fs ...)
     (pipe (f in args ...) fs ...))
    ((pipe in f fs ...)
     (pipe (f in) fs ...))))

(define-syntax pipe*
  (syntax-rules ()
    ((pipe* in)
     in)
    ((pipe* in (f args ...) fs ...)
     (pipe* (f args ... in) fs ...))
    ((pipe* in f fs ...)
     (pipe* (f in) fs ...))))

(define-syntax let-if
  (syntax-rules ()
    ((let-if (name expr pred)
             consequent
             alternative)
     (let ((name expr))
       (if (pred name)
           consequent
           alternative)))
    ((let-if (name expr)
             consequent
             alternative)
     (let-if (name expr identity)
             consequent
             alternative))))

(ignore
  (put 'let-if 'scheme-indent-function 1))

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
       (lambda formals body ...)))))

(ignore
  (put 'receive 'scheme-indent-function 2))

;; Predicates

(define (list-of pred xs)
  (every pred xs))
