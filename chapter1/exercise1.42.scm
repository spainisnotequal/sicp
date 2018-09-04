;; ============= ;;
;; Exercise 1.42 ;;
;; ============= ;;


(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x)
  (+ x 1))

((compose square inc) 6) ;; should be 7