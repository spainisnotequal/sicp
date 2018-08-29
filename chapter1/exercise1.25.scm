;; ====================== ;;
;; Exercises of Chapter 1 ;;
;; ====================== ;;

;; ------------- ;;
;; Exercise 1.25 ;;
;; ------------- ;;


;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (square x)
  (* x x))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;; Execution:

(define start-time (runtime))

(define ntimes 10000)

(newline)
(display "Is 3 a prime number? ")
(display (fast-prime? 3 ntimes))
(newline)

(newline)
(display "Is 4 a prime number? ")
(display (fast-prime? 4 ntimes))
(newline)

(newline)
(display "Is 7 a prime number? ")
(display (fast-prime? 7 ntimes))
(newline)

(newline)
(display "Is 8 a prime number? ")
(display (fast-prime? 8 ntimes))
(newline)


(define elapsed-time (- (runtime) start-time))
(newline)
(newline)
(display "Elapsed time (sec) = ")
(display elapsed-time)
(newline)
(newline)
