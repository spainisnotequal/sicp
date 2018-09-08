;; ============= ;;
;; Exercise 2.20 ;;
;; ============= ;;


(define (same-parity x . y)
  (let ((check-parity (if (odd? x)
			  odd?
			  even?)))
    (define (iter items ans)
      (if (null? items)
	  ans
	  (if (check-parity (car items))
	      (iter (cdr items) (append ans (list (car items))))
	      (iter (cdr items) ans))))
    (iter y (list x))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
