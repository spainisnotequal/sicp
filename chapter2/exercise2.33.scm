;; ============= ;;
;; Exercise 2.33 ;;
;; ============= ;;

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
	    (accumulate proc initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      '()
	      sequence))

(map square (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 4))

(define (length sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0 sequence))

(length (list 1 2 3 4 3 2 1))
