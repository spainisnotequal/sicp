;; ============ ;;
;; Exercise 2.3 ;;
;; ============ ;;



;; "Point" data structure:

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))


;; -------------------- ;;
;; First representation ;;
;; -------------------- ;;

;; A rectangle is defined by its south-west point, its width and its height

(define (make-rectangle sw-point width height)
  (cons sw-point (cons width height)))

(define (rect-sw-point rect)
  (car rect))

(define (rect-width rect)
  (car (cdr rect)))

(define (rect-height rect)
  (cdr (cdr rect)))


(define (rect-perimeter rect)
  (+ (* 2 (rect-width rect))
     (* 2 (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect)
     (rect-height rect)))


;; Example:

(define rect (make-rectangle (make-point 0 0) 4 2))

(rect-perimeter rect)
(rect-area rect)


;; --------------------- ;;
;; Second representation ;;
;; --------------------- ;;

;; A rectangle is defined by two opposite points

(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (rect-p1 rect)
  (car rect))

(define (rect-p2 rect)
  (cdr rect))

(define (rect-width rect)
  (abs (- (x-point (rect-p1 rect))
	  (x-point (rect-p2 rect)))))

(define (rect-height rect)
  (abs (- (y-point (rect-p1 rect))
	  (y-point (rect-p2 rect)))))


(define (rect-perimeter rect)
  (+ (* 2 (rect-width rect))
     (* 2 (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect)
     (rect-height rect)))


;; Example:

(define rect (make-rectangle (make-point 0 0) (make-point 4 2)))

(rect-perimeter rect)
(rect-area rect)
