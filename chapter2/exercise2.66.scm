;; ============= ;;
;; Exercise 2.66 ;;
;; ============= ;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (make-record key name age)
  (list key name age))
(define (key record) (car record))
(define (name record) (cadr record))
(define (age record) (caddr record))


(define (lookup given-key set)
  (cond ((null? set) false)
	((= given-key (key (entry set))) (entry set))
	((< given-key (key (entry set))) (lookup given-key (left-branch set)))
	(else (lookup given-key (right-branch set)))))


;; ----- ;;
;; Tests ;;
;; ----- ;;

(define tree-db (list->tree (list (make-record 1 'jose 40)
			      (make-record 2 'maria 35)
			      (make-record 3 'jesus 21))))

(lookup 1 tree-db) ; (1 jose 40)
(lookup 2 tree-db) ; (2 maria 35)
(lookup 3 tree-db) ; (3 jesus 21)
(lookup 4 tree-db)


;; ---------------------------------------- ;;
;; Other procedures from previous exercises ;;
;; ---------------------------------------- ;;


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
