;; ============ ;;
;; Exercise 3.2 ;;
;; ============ ;;


(define (make-monitored f)
  (let ((counter 0))
    (lambda(m)
      (cond ((eq? m 'how-many-calls?) counter)
	    ((eq? m 'reset-count) (set! counter 0))
	    (else
	     (set! counter (+ counter 1))
	     (f m))))))

(define s (make-monitored sqrt))

(s 'how-many-calls?)
(s 100)
(s 'how-many-calls?)
(s 100)
(s 'reset-count)
(s 'how-many-calls?)
