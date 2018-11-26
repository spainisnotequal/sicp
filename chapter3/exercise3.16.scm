;; ============= ;;
;; Exercise 3.16 ;;
;; ============= ;;

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
;; the previous procedure is not correct, because it counts shared data structures many times.
;; For example:
(define p1 (cons 'a 'b))
(count-pairs p1)
(define l1 (list p1 p1))
(count-pairs l1) ;=> 4, when it should be 3 (car of both pairs of the list points to the pair p1, which is counted twice as if it was two different pairs)


;; Write list structures made up of exactly three pairs for which that procedure would return: 3; 4; 7; and never return at all:

(define l2 (list 'a 'b 'c))
(count-pairs l2) ;=> 3

(define l3 (list l2))
(count-pairs l3) ;=> 4

(define l4 (list l2))
(set-cdr! l4 l2)
(count-pairs l4) ;=> 7

(define l5 (list l2))
(set-cdr! l5 l5)
(count-pairs l5) ;=> Aborting!: maximum recursion depth exceeded
