;; ============= ;;
;; Exercise 2.27 ;;
;; ============= ;;

(define (reverse a-list)
  (if (null? a-list)
      '()
      (append (reverse (cdr a-list))
	      (list (car a-list)))))

(define (deep-reverse a-list)
  (cond ((null? a-list) '())
	((not (pair? a-list)) a-list)
	(else (append (deep-reverse (cdr a-list))
		      (list (deep-reverse (car a-list)))))))


(define x (list (list 1 2) (list 3 4)))
x ; ((1 2) (3 4))
(reverse x) ; ((3 4) (1 2))
(deep-reverse x) ; ((4 3) (2 1))

(define y (list (list 1 2 3) (list 4 5) (list 6 (list 7 8) 9)))
y ; ((1 2 3) (4 5) (6 (7 8) 9))
(deep-reverse y) ; ((9 (8 7) 6) (5 4) (3 2 1))
