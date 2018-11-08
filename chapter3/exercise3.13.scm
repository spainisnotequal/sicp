;; ============= ;;
;; Exercise 3.13 ;;
;; ============= ;;

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define x '(a b c))

(define z (make-cycle x))

(last-pair z) ; infinite recursion trying to find the last pair of z
