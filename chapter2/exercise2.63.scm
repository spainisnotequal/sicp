;; ============= ;;
;; Exercise 2.63 ;;
;; ============= ;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; Order of growth: n*log(n)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;; Order of growth: n
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define b1 (make-tree 1 '() '()))
(define b5 (make-tree 5 '() '()))
(define b7 (make-tree 7 '() '()))
(define b9 (make-tree 9 '() '()))
(define b11 (make-tree 11 '() '()))

(define t1 (make-tree 7
		      (make-tree 3 b1 b5)
		      (make-tree 9 '() b11)))

(define t2 (make-tree 3
		      b1
		      (make-tree 7
				 b5
				 (make-tree 9 '() b11))))

(define t3 (make-tree 5
		      (make-tree 3 b1 '())
		      (make-tree 9 b7 b11)))

(tree->list-1 t1)
(tree->list-2 t1)

(tree->list-1 t2)
(tree->list-2 t2)

(tree->list-1 t3)
(tree->list-2 t3)
