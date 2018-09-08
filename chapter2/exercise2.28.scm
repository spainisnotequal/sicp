;; ============= ;;
;; Exercise 2.28 ;;
;; ============= ;;

(define (fringe a-list)
  (cond ((null? a-list) '())
	((not (pair? a-list)) (list a-list))
	(else (append (fringe (car a-list))
		      (fringe (cdr a-list))))))


(define x (list (list 1 2) (list 3 4)))
x ; ((1 2) (3 4))

(fringe x) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)
