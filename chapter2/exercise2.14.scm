;; ============= ;;
;; Exercise 2.14 ;;
;; ============= ;;


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
		       (div-interval one r2)))))


;; Examples

(define x (make-center-percent 6.8 1))
(define y (make-center-percent 4.7 5))

(newline)
(display "par1 = ")
(display (par1 x y))
(newline)
(display "par2 = ")
(display (par2 x y))
(newline)


(define x-by-x (div-interval x x))

(newline)
(display "x / x = ")
(display x-by-x)
(newline)
(display "center = ")
(display (center x-by-x))
(newline)
(display "percent = ")
(display (percent x-by-x))
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

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100.0)))
		 (+ c (* c (/ p 100.0)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* 100.0 (- 1 (/ (lower-bound i) (center i)))))
