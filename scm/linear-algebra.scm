(define (dot-product vec1 vec2)
  (reduce + 0 (map * vec1 vec2)))

(define (transpose mat)
  (if (any null? mat)
      '()
      (cons (map car mat)
            (transpose (map cdr mat)))))


(define (make-matrix rows cols elms)
  (define (group-rows elms)
    (if (null? elms)
        null
        (cons (take elms cols)
              (group-rows (drop elms cols)))))
  (if (not (= (* rows cols) (length elms)))
      (error 'matrix "Dimension mismatch" rows cols)
      (transpose (group-rows elms))))

(define-syntax matrix
  (syntax-rules ()
    ((matrix (rows cols)
        elm ...)
     (make-matrix rows cols (quote (elm ...))))))

(ignore
 (put 'matrix 'scheme-indent-function 1))


(define M1
  (matrix (5 2)
    1 6
    2 7
    3 8
    4 9
    5 10))
