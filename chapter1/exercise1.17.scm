;; ====================== ;;
;; Exercises of Chapter 1 ;;
;; ====================== ;;

;; ------------- ;;
;; Exercise 1.17 ;;
;; ------------- ;;

(define (* a b)
  (cond ((= b 0) 0)
	((= b 1) a)
	((even? b) (double (* a (halve b))))
	(else (+ (* a (- b 1)) a))))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(* 2 0) ; 0
(* 3 11) ;33
