;; ============= ;;
;; Exercise 2.65 ;;
;; ============= ;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
	((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))


;; Tree to list:
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; List to tree:
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


;; ----- ;;
;; Union ;;
;; ----- ;;

(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((and (null? list1) (null? list2)) '())
	  ((null? list1) list2)
	  ((null? list2) list1)
	  ((= (car list1) (car list2)) (union-list list1 (cdr list2)))
	  ((< (car list1) (car list2)) (cons (car list1) (union-list (cdr list1) list2)))
	  (else (cons (car list2) (union-list list1 (cdr list2))))))
  (list->tree (union-list (tree->list-2 set1)
			  (tree->list-2 set2))))


;; ------------ ;;
;; Intersection ;;
;; ------------ ;;

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
	'()
	(let ((x1 (car list1))
	      (x2 (car list2)))
	  (cond ((= x1 x2)
		 (cons x1 (intersection-list (cdr list1) (cdr list2))))
		((< x1 x2) (intersection-list (cdr list1) list2))
		(else (intersection-list list1 (cdr list2)))))))
  (list->tree (intersection-list (tree->list-2 set1)
                                 (tree->list-2 set2))))

;; ----- ;;
;; Tests ;;
;; ----- ;;

(define b1 (make-tree 1 '() '()))
(define b3 (make-tree 3 '() '()))
(define b5 (make-tree 5 '() '()))
(define b6 (make-tree 6 '() '()))
(define b7 (make-tree 7 '() '()))
(define b9 (make-tree 9 '() '()))
(define b11 (make-tree 11 '() '()))

(define t1 (make-tree 7
		      (make-tree 3 b1 b5)
		      (make-tree 9 '() b11)))

(define t2 (make-tree 5
		      (make-tree 2 '() b3)
		      (make-tree 9 b6 b11)))
(tree->list-2 t1)
(tree->list-2 t2)

(union-set t1 t2)
(intersection-set t1 t2)
