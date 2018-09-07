;; ============ ;;
;; Exercise 2.8 ;;
;; ============ ;;

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))


(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define x (make-interval 2 8))
(define y (make-interval 1 5))

(sub-interval x y)
