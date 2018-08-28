;; ====================== ;;
;; Exercises of Chapter 1 ;;
;; ====================== ;;

;; ------------ ;;
;; Exercise 1.7 ;;
;; ------------ ;;


;; For very small numbers, it is not possible to find any square root that is smaller than the tolerance, because in that case the difference between tha last guess and the new one would always be smaller so the "good-enough?" will return #t (true) and the guess won't be improved anymore.
;; For example: when computing the square root of 0.00000001 (which real solution is 0.0001), the last two guesses that the program is able to compute are 0.001954831361974762 and 0.0009799734463768973, so the solution would be 0.001954831361974762, which is wrong by an order of magnitude.

;; Due to the limited precision of numbers, we won't be able to represent very large numbers with enough decimals (the fractional o decimal part of a real number, as opposed to its integer part), so the difference between the old guess and the new one will never be smaller than the tolerance, and the program will be trap in an infinite loop, because the "good-enough?" procedure will return #f (false) but, at the same time, won't be able to produce new guesses with enough precision in the fractional o decimal part of the value of these new guesses.
;; For example: when computing the square root of 987654321098765432 (which real solution is 993807990055808.2) the programs enters in the infinite loop mentioned before.


;; The solution I propose contemplates two modifications in the "good-enough?" procedure:
;;     1) compute the change between guesses, so we can stop the procedure when the change is smaller than a tolerance; and
;;     2) define the tolerance as a percentange of the guess, not as an absolute value.

;; This improves the solution for very small numbers but no so for very large ones...

;; So the modified version of the square roots by Newton's method would be:

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x)))
     (* 0.001 guess))) ;; stop the algorithm when the change between guesses is less than 0.1% of the last guess.

(define (sqrt x)
  (sqrt-iter 1.0 x))
