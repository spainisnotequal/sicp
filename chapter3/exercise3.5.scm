;; ============ ;;
;; Exercise 3.5 ;;
;; ============ ;;

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* 1.0
     (area-rect x1 x2 y1 y2)
     (monte-carlo trials
		  (lambda ()
		    (p x1 x2 y1 y2)))))

(define (area-rect x1 x2 y1 y2)
  (* (- x2 x1)
     (- y2 y1)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (inside-circle? x1 x2 y1 y2)
  (let* ((x (random-in-range x1 x2))
	 (y (random-in-range y1 y2))
	 (x0 (average x1 x2))
	 (y0 (average y1 y2))
	 (r (abs (- x2 x0))))
    (<= (+ (square (- x x0))
    	   (square (- y y0)))
     	(square r))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

;; Estimation of pi by measuring the area of a unit circle
(estimate-integral inside-circle? -1.0 1.0 -1.0 1.0 1000)
(estimate-integral inside-circle? -1.0 1.0 -1.0 1.0 1000000)
