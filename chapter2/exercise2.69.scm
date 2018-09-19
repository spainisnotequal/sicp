;; ============= ;;
;; Exercise 2.69 ;;
;; ============= ;;

;; ------------------------------ ;;
;; Leaf constructor and selectors ;;
;; ------------------------------ ;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; -------------------------------------- ;;
;; Huffman tree constructor and selectors ;;
;; -------------------------------------- ;;

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; --------------------- ;;
;; Generate Huffman-Tree ;;
;; --------------------- ;;

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge (adjoin-set (make-code-tree (car set)
						    (cadr set))
				    (cddr set)))))

;; Tests:

(define pairs '((a 4) (b 2) (c 1) (d 1)))
(generate-huffman-tree pairs)
