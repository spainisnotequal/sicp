;; ============= ;;
;; Exercise 2.13 ;;
;; ============= ;;


(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100.0)))
		 (+ c (* c (/ p 100.0)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* 100.0 (- 1 (/ (lower-bound i) (center i)))))

;; Examples

(define x (make-center-percent 6.8 1))
(define y (make-center-percent 4.7 5))

(newline)
(display "The tolarence of the product of two intervals is approximately equal to the sum of the tolerance of one interval an the tolerance of the other")
(newline)
(display "Tolerance of the product = ")
(display (percent (mul-interval x y)))
(newline)	 
(display "Sum of the tolerances    = ")
(display (+ (percent x) (percent y)))
(newline)
(display "Difference               = ")
(display (- (percent (mul-interval x y))
	    (+ (percent x) (percent y))))
(newline)


;; From previous exercises:

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (width y) 0)
      (error " ERROR: You are trying to divide by an interval that spans zero:" y)
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x))
     2))

