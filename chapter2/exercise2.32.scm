;; ============= ;;
;; Exercise 2.32 ;;
;; ============= ;;

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define s1 (list 1))
(define s2 (list 1 2))
(define s3 (list 1 2 3))

(subsets s1)
(subsets s2)
(subsets s3)
