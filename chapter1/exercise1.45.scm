;; ============= ;;
;; Exercise 1.45 ;;
;; ============= ;;

;; Fixed point procedure:

(define tolerance 1e-10)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;; Average damp procedure:

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))


;; More general fixed point procedure:

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; Repeated procedure:

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))


;; Experiments on how many average damps are necesary

(define (nth-root n x damp-times)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp damp-times)
                            1.0))
(define (how-many-damps damps x)
  (define (iter n)
    (cond ( (< n 20) (and (newline)
			  (display (nth-root n (expt x n) damps))
			  (iter (+ n 1))))
	  (else (and (newline)
		     (display "----------------- Last n-th root computed: ")
		     (display n)
		     (newline)))))
  (iter 1))

; (how-many-damps 1 5) ;; 1 average damp works until the 3rd-root
; (how-many-damps 2 5) ;; 2 average damps work until the 7th-root
; (how-many-damps 3 5) ;; 3 average damps work until the 15th-root


;; General form to compute the n-th root

(define (nth-root n x)
  (let ((ndamps (floor (/ (log n) (log 2)))))
    (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
			      (repeated average-damp ndamps)
			      1.0)))


(define (roots n x)
  (if (< n 40)
      (and (newline)
	   (display n)
	   (display "-th root of ")
	   (display x)
	   (display " = ")
	   (display (nth-root n (expt x n)))
	   (roots (+ n 1) x))
      (and (newline)
	   (display "----------------- Last n-th root computed: ")
	   (display n)
	   (newline))))

(roots 2 5)

;; The nth-roots works (converges) until the 22th root. I don't know why...
