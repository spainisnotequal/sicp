;; ======================================== ;;
;; Section 3.3: Modelling with Mutable Data ;;
;; ======================================== ;;

;; ------------------ ;;
;; Share and identity ;;
;; ------------------ ;;

(define x (list 'a 'b))

(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(eq? (car z1) (cdr z1)) ;=> #t (same object)
(eq? (car z2) (cdr z2)) ;=> #f (different objects)

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
(set-to-wow! z1) ;=> ((wow b) wow b)

z2
(set-to-wow! z2) ;=> ((wow b) a b)
