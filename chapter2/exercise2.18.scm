;; ============= ;;
;; Exercise 2.18 ;;
;; ============= ;;


(define (reverse a-list)
  (if (null? a-list) ; (= (length a-list) 1)
      '()
      (append (reverse (cdr a-list))
	    (list (car a-list)))))

(reverse (list 1 4 9 16 25))
