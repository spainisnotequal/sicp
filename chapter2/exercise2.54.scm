;; ============= ;;
;; Exercise 2.54 ;;
;; ============= ;;


(define (equal? a b)
  (cond ((and (null? a) (null? b)) true)
	((and (symbol? a) (symbol? b)) (eq? a b))
	((and (list? a) (list? b)) (and (equal? (car a) (car b))
					(equal? (cdr a) (cdr b))))
	(else false)))


(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
