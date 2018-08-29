;; ====================== ;;
;; Exercises of Chapter 1 ;;
;; ====================== ;;

;; ------------- ;;
;; Exercise 1.11 ;;
;; ------------- ;;


;; Recursive Process:

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* (f (- n 2)) 2)
	 (* (f (- n 3)) 3))))

;; Iterative Process:

(define (f n)
  (f-iter 2 1 0 n)))

(define (f-iter a b c count)
   (if (< count 3)
       a
       (f-iter (+ a (* 2 b) (* 3 c))
               a
               b
               (- count 1))))
