;; ============= ;;
;; Exercise 1.44 ;;
;; ============= ;;

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))


(define (smooth f)
  (let ((dx 0.1))
    (lambda (x) (/ (+ (f (- x dx))
		      (f x)
		      (f (+ x dx)))
		   3))))

(define (smooth-n-times f n)
  (repeated (smooth f) n))

((smooth-n-times square 2) 5)
