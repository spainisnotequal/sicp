;; ============= ;;
;; Exercise 2.53 ;;
;; ============= ;;

(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))


'((x1 x2) (y1 y2)) ; ((x1 x2) (y1 y2))
(quote ((x1 x2) (y1 y2))) ; ((x1 x2) (y1 y2))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)


(car '(a short list)) ; a
(pair? (car '(a short list))) ; #f


(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))
