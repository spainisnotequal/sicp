;; ============= ;;
;; Exercise 2.38 ;;
;; ============= ;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (foo op initial sequence)
  (if (null? sequence)
      initial
      (op (accumulate op initial (cdr sequence))
	  (car sequence))))

(fold-right / 1 (list 1 2 3)) ; same as: (accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))
(fold-right cons '() (list 1 2 3))
(fold-left cons '() (list 1 2 3))
