;; ============= ;;
;; Exercise 2.25 ;;
;; ============= ;;

(define a (list 1 3 (list 5 7) 9)) ; (1 3 (5 7) 9)
(car (cdr (car (cdr (cdr a)))))
(car (cdaddr a))
;; sadly, this expression does not work: (cadaddr a)

(define b (list (list 7))) ; ((7))
(car (car b))
(caar b)

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))) ; (1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
(cadr (cadr (cadr (cadr (cadr (cadr c))))))
