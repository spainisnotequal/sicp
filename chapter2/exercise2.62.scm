;; ============= ;;
;; Exercise 2.62 ;;
;; ============= ;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
	((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2) (intersection-set (cdr set1) set2))
	      (else (intersection-set set1 (cdr set2)))))))


(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))


(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
	((null? set1) set2)
	((null? set2) set1)
	((= (car set1) (car set2)) (union-set set1 (cdr set2)))
	((< (car set1) (car set2)) (cons (car set1)
					 (union-set (cdr set1) set2)))
	(else (cons (car set2) (union-set set1 (cdr set2))))))
	

(define set1 '(3 5 7))
(define set2 '(2 4))

(element-of-set? 3 set1)
(element-of-set? 2 set1)

(intersection-set set1 set2)
(intersection-set set1 '(1 2 3))

(adjoin-set 1 '())
(adjoin-set 1 set1)
(adjoin-set 4 set1)
(adjoin-set 9 set1)

(union-set set1 set2)
