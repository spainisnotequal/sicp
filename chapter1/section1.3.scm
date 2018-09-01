;; ================================================================== ;;
;; Section 1.3: Formulating Abstractions with Higher-Order Proceudres ;;
;; ================================================================== ;;


;; ----------- ;;
;; Sum Example ;;
;; ----------- ;;


;; Higher-order sum procedure:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))


;; Sum of integers:

(define (identity x)
  x)

(define (inc n)
  (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 2 5)


;; Sum of cubes:

(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 3)


;; Pi function sum:

(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))

(define (pi-next x)
  (+ x 4))

(define (pi-sum a b)
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))


;; ---------------------------------------------- ;;
;; Roots of equations by the half-interval method ;;
;; ---------------------------------------------- ;;

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define tolerance 0.001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (average x y)
  (/ (+ x y) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(newline)
(newline)
(display "root of sin between [2.0, 4.0] = ")
(display (half-interval-method sin 2.0 4.0))
(newline)
(display "root of x³-2x-3 between [1.0, 2.0] = ")
(display (half-interval-method (lambda (x)
				 (- (* x x x) (* 2 x) 3))
			       1.0
			       2.0))
(newline)


;; ------------------------- ;;
;; Fixed points of functions ;;
;; ------------------------- ;;

(define tolerance 0.00001)


;; Book's version:

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(newline)
(display "Book's version:")
(newline)
(display "cos(1.0) = ")
(display (fixed-point cos 1.0))
(newline)
(display "siny + cos y (1.0) = ")
(display (fixed-point (lambda (y) (+ (sin y) (cos y)))
			       1.0))
(newline)


;; My version:

(define (close-enough? x y)
  (< (abs (- x y)) tolerance)) ; I define this procedure outside the main procedure because: (1) it can be useful in other prodecures; (2) it makes the code easier to read

(define (fixed-point f guess)
  (let ((next (f guess)))
    (if (close-enough? guess next)
	next ; or "guess", because they are close enough
	(fixed-point f next))))

(newline)
(display "My version:")
(newline)
(display "cos(1.0) = ")
(display (fixed-point cos 1.0))
(newline)
(display "siny + cos y (1.0) = ")
(display (fixed-point (lambda (y) (+ (sin y) (cos y)))
			       1.0))
(newline)
(newline)


;; ------------------------------ ;;
;; Procedures as first class data ;;
;; ------------------------------ ;;

;; In List, procedures are first class data, so we can:
;;     - give them a name;
;;     - use them as arguments to other porcedures;
;;     - return them as the result of the application of other procedures; and
;;     - include them in data structures.

;; Let's see how we can:
;;     - use procedures as arguments of other procedures; and
;;     - obtain procedures as the result of the application of other procedures.

;; We'll compute the square root function using different approaches:
;;     (1) Fixed-point search method.
;;     (2) Newton's method.
;;     (3) An even more general fixed-point search method, that we'll use to compute the square root as:
;;         (3.1) y  |->  x/y   (using "average-damp")
;;         (3.1) y  |->  y²-x  (using "newton-transform")


;; (1) Fixed-point search method
;; -----------------------------

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt1 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(newline)
(newline)
(display "Compute the square root of 7 by using 4 different approaches")
(newline)
(display "Fixed-point search = ")
(display (sqrt1 7))
(newline)


;; (2) Newton's method
;; -------------------

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt2 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(display "Newton's method = ")
(display (sqrt2 7))
(newline)


;; (3.1) General fixed-point search using average dumping
;; ------------------------------------------------------

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(display "General fixed-point search (average dumping) = ")
(display (sqrt3 7))
(newline)


;; (3.1) General fixed-point search using average dumping
;; ------------------------------------------------------

(define (sqrt4 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(display "General fixed-point search (Newton tranformation) = ")
(display (sqrt4 7))
(newline)
(newline)
