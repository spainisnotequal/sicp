;; ======================================================= ;;
;; Section 2.2: Hierarchical Data and the Closure Property ;;
;; ======================================================= ;;

;; From Figure 2.3: Two ways to combine 1 2 3 4:
(define a (cons (cons 1 2) (cons 3 4)))
(define b (cons (cons 1 (cons 2 3)) 4))
a ; ((1 . 2) 3 . 4)
b ; ((1 2 . 3) . 4)

;; Some other options:
(define c (cons 1 (cons 2 (cons 3 4))))
(define d (cons (cons (cons 1 2) 3) 4))
c ; (1 2 3 . 4)
d ; (((1 . 2) . 3) . 4)

;; Most common option:
(define e (cons 1 (cons 2 (cons 3 (cons 4 '())))))
e ; (1 2 3 4)


;; --------------- ;;
;; List operations ;;
;; --------------- ;;

(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))

(car one-through-four) ; 1
(cdr one-through-four) ; (2 3 4)
(car (cdr one-through-four)) ; 2
(cdr (cdr one-through-four)) ; (3 4)
(cdr (cdr (cdr (cdr one-through-four)))) ; ()

(list-ref one-through-four 0) ; 1
(list-ref one-through-four 1) ; 2
(list-ref one-through-four 2) ; 3
(list-ref one-through-four 3) ; 4
(list-ref one-through-four 4) ; The object 4, passed as an argument to list-ref, is not in the correct range.

(length one-through-four) ; 4

(define odds (list 1 3 5 7 9))

(append one-through-four odds) ; (1 2 3 4 1 3 5 7 9)


;; ------------------ ;;
;; Mapping over lists ;;
;; ------------------ ;;


(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)


;; "map" general procedure provided by the Scheme implementation
;; -------------------------------------------------------------

;; using it with a single list:
(map (lambda(x) (* x 10)) (list 1 2 3 4 5))
(map (lambda(x) (* x x)) (list 1 2 3 4 5))
(map abs (list 1 -2 -3 4 5))

;; using it with two or  more lists:
(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))


;; "map" procedure for a single list can be implemented this way
;; -------------------------------------------------------------

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x)) (list 1 2 3 4))


;; rewritting the "scale-list" procedure in terms of "map"
;; -------------------------------------------------------

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)
