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

;; Most common option (called a "list" in Lisp terminology):
(define e (cons 1 (cons 2 (cons 3 (cons 4 '())))))
e ; (1 2 3 4)

;; See the differences and how important it is!:
(car a) ; (1 2)
(cdr a) ; (3 4)

(car b) ; (1 (2 3))
(cdr b) ; 4

(car e) ; 1
(cdr e) ; (2 3 4)


;; ---------------------------- ;;
;; 2.2.1 Representing Sequences ;;
;; ---------------------------- ;;


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


;; more examples about how to use map:

;; "sqrt" procedure only accepts one argument, so only one list:
(map sqrt (list 1 4 9))

;; "expt" procedure accepts two arguments, so two lists:
(map expt (list 1 2 3) (list 4 5 6))
(map expt (list 1 2 3) 3) ; error
(map expt (list 1 2 3) (list 3 3)) ; not an error, but only computes the first two elements
(map expt (list 1 2 3) (list 3 3 3))

;; "+" procedure accepts one or more arguments:
(map + (list 1 2 3) (list 4 5 6))
(map + (list 1 2 3) (list 4 5 6) (list 7 8 9))
(map + (list 1 2 3)) ; not an error, because "+" accepts one argument as well


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


;; ----------------------------- ;;
;; 2.2.2 Hierarchical Structures ;;
;; ----------------------------- ;;


(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4))) ; ((1 2) 3 4)

(length x) ; 3
(count-leaves x) ; 4

(list x x)
(length (list x x)) ; 2
(count-leaves (list x x)) ; 8 


;; ------------------ ;;
;; Mapping over trees ;;
;; ------------------ ;;


(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define a-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(scale-tree a-tree 10)

;; using "map" and recursion, we can implement "scale-tree" as:

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree a-tree 10)


;; ------------------------------------------ ;;
;; 2.2.3 Sequences as Conventional Interfaces ;;
;; ------------------------------------------ ;;

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree)) (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(sum-odd-squares (cons 1 (cons (cons 2 5) 3)))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(even-fibs 9)


;; Redifining those procedures folloing the workflow represented in Figure 2.7:


;; Map:  (already defined in section 2.2.1)
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map square (list 1 2 3 4 5))

;; Filter:
(define (filter pred seq)
  (cond ((null? seq) '())
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(filter even? (list 1 2 3 4 5))
(filter odd? (list 1 2 3 4 5))

;; Accumulate:
(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
	    (accumulate proc initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))

;; Enumerate:

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

;; Redifinition of "sum-odd-squares":
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares (cons 1 (cons (cons 2 5) 3)))

;; Redefinition of "even-fibs":
(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(even-fibs 9)

;; Other procedures defined using this general procedures: map, filter, accumulate,...

(define (list-fib-squares n)
  (accumulate cons
              '()
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))
