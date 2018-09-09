;; ============= ;;
;; Exercise 2.30 ;;
;; ============= ;;

;; Without using any higher-order procedure:

(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define a-tree  (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree a-tree)


;; Using "map" and recursion:

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

(square-tree a-tree)
