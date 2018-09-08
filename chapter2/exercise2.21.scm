;; ============= ;;
;; Exercise 2.21 ;;
;; ============= ;;

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(square-list (list 1 3 5))

(define (square-list items)
  (map square items))

(square-list (list 1 3 5))
