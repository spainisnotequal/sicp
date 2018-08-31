;; ============= ;;
;; Exercise 1.37 ;;
;; ============= ;;


;; ----------------------------------------------- ;;
;; 1.37 (a) Continued fraction (recursive process) ;;
;; ----------------------------------------------- ;;

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
	(/ (n i) (+ (d i) (frac (+ i 1))))
	(/ (n i) (d i))))
  (frac 1))

;; Compute the golden ratio (which is aprox. 1.6180) with an accuracy of 4 decimal places

(define (golden-ratio k)
  (/ 1 (cont-frac (lambda (i) 1.0)
		  (lambda (i) 1.0)
		  k)))

(newline)
(display "golden ratio (k=5) =  ")
(display (golden-ratio 5))
(newline)
(display "golden ratio (k=10) = ")
(display (golden-ratio 10))
(newline)
(display "golden ratio (k=20) = ")
(display (golden-ratio 20))
(newline)	


;; ----------------------------------------------- ;;
;; 1.37 (b) Continued fraction (iterative process) ;;
;; ----------------------------------------------- ;;

(define (cont-frac n d k)
  (define (frac-iter i result)
    (if (= i 0)
	result
	(frac-iter (- i 1)
		   (/ (n i) (+ (d i) result)))))
  (frac-iter k 0))


(define (golden-ratio k)
  (/ 1 (cont-frac (lambda (i) 1.0)
		  (lambda (i) 1.0)
		  k)))

(newline)
(display "golden ratio (k=7) =  ")
(display (golden-ratio 7))
(newline)
(display "golden ratio (k=12) = ")
(display (golden-ratio 12))
(newline)
(display "golden ratio (k=17) = ")
(display (golden-ratio 17))
(newline)
(newline)
