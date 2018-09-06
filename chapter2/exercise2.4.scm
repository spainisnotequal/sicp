;; ============ ;;
;; Exercise 2.4 ;;
;; ============ ;;


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define p1 (cons 2 3))
(car p1)
(cdr p1)
