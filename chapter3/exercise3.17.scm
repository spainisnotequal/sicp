;; ============= ;;
;; Exercise 3.17 ;;
;; ============= ;;

(define (count-pairs x)
  (let ((counted '())) 
    (define (helper x) 
      (cond ((not (pair? x)) 0)
            ((memq x counted) 0)
            ((set! counted (cons x counted))
             (+ (helper (car x)) 
                (helper (cdr x)) 
                1)))) 
    (helper x)))

(define p1 (cons 'a 'b))
(count-pairs p1)
(define l1 (list p1 p1))
(count-pairs l1) ;=> 3 (in the previous exercise the result was 4)

(define l2 (list 'a 'b 'c))
(count-pairs l2) ;=> 3

(define l3 (list l2))
(count-pairs l3) ;=> 4

(define l4 (list l2))
(set-cdr! l4 l2)
(count-pairs l4) ;=> 4 (in the previous exercise the result was 7)
