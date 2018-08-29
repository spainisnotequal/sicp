;; ====================== ;;
;; Exercises of Chapter 1 ;;
;; ====================== ;;

;; ------------- ;;
;; Exercise 1.23 ;;
;; ------------- ;;


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;; Timed prime test:

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (* (- (runtime) start-time) 1000000))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display (* 1000 elapsed-time))
  (display " (msec)"))


(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ start 1) end)
      (if (< start end)
	  (and (timed-prime-test start) (search-for-primes (+ start 2) end)))))


;; Execution:

(define start-time (runtime))

(search-for-primes 0 1000)

(define elapsed-time (- (runtime) start-time))
(newline)
(newline)
(display "Elapsed time (sec) = ")
(display elapsed-time)
(newline)
(newline)
