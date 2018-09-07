;; ============ ;;
;; Exercise 2.6 ;;
;; ============ ;;


(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

((zero add-1) 0) ; 0
((zero add-1) 1) ; 1
((zero add-1) 5) ; 5


;; ----------------------------------------------------------------------- ;;
;; (a) Define "one" and "two" directly (not in terms of "zero" and "add-1" ;;
;; ----------------------------------------------------------------------- ;;

;; First I use substitution to evaluate (add-1 zero) to see the evolution of the process

    ;; (add-1 zero)

    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f ((zero f)
    ;; 	x))))

    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f ((lambda (f)
    ;; 	  (lambda (x)
    ;; 	    x)
    ;; 	  f)
    ;; 	x))))

    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f ((lambda (x)
    ;; 	  x)
    ;; 	x))))

    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f x)))

;; So the last expressions are the body of the definition of "one"

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

;; Similarly, using substitution to evaluate (add-1 one) 

    ;; (add-1 one)

    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f ((one f)
    ;; 	x))))

    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f ((lambda (f)
    ;; 	  (lambda (x)
    ;; 	    (f x)))
    ;; 	f)
    ;;        x)))


    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f ((lambda (x)
    ;; 	  (f x))
    ;; 	x))))

    ;; (lambda (f)
    ;;   (lambda (x)
    ;;     (f (f x))))

;; So the last expressions are the body of the definition of "two"

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))


;; Examples:

((one add-1) 3) ; 4
((two add-1) 5) ; 7
((zero add-1) 3) ; 3


;; -------------------------------------------------------- ;;
;; (b) Give a direct definition of the addition procedure + ;;
;; -------------------------------------------------------- ;;


(define (+ a b)
  (lambda (f)
    (lambda (x)
      ((a f)
       ((b f)
	x)))))

(define three
  (+ one two))

((three add-1) 0) ; 3
((three add-1) 5) ; 8
