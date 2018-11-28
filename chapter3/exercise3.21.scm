;; ============= ;;
;; Exercise 3.21 ;;
;; ============= ;;

(define (print-queue q)
  (front-ptr q))

(define q (make-queue))
(print-queue q)
(insert-queue! q 'a) ;=> ((a) a)
(print-queue q)      ;=> (a)
(insert-queue! q 'b) ;=> ((a b) b)
(print-queue q)      ;=> (a b)
(delete-queue! q)    ;=> ((b) b)
(print-queue q)      ;=> (b)
(delete-queue! q)    ;=> (() b)
(print-queue q)      ;=> ()
