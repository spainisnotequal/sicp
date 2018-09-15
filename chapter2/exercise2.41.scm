;; ============= ;;
;; Exercise 2.41 ;;
;; ============= ;;

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
	    (accumulate proc initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (ordered-triplets n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
		    (map (lambda (k) (list k j i))
			 (enumerate-interval 1 (- j 1))))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(ordered-triplets 6)


(define (sum-equals-s? triplet s)
  (= (+ (car triplet) (cadr triplet) (caddr triplet))
     s))

(sum-equals-s? (list 1 2 3) 6)
(sum-equals-s? (list 1 2 4) 6)

;; Procedure that finds all ordered triplets less or equal to n that sum s

(define (s-sum-triplets n s)
  (filter (lambda (triplet)
	    (sum-equals-s? triplet s))
	  (ordered-triplets n)))

(s-sum-triplets 6 9)
