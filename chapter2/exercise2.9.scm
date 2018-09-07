;; ============ ;;
;; Exercise 2.9 ;;
;; ============ ;;

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))


(define (width x)
  (/ (- (upper-bound x) (lower-bound x))
     2))


(define x (make-interval 2 8))

(width x)
