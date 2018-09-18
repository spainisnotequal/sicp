;; ============= ;;
;; Exercise 2.67 ;;
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

;; Tests:

(define leaf1 (make-leaf 'A 8))
(leaf? leaf1)
(symbol-leaf leaf1)
(weight-leaf leaf1)

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

;; Tests:

(define lA (make-leaf 'A 8))
(define lB (make-leaf 'B 3))
(define lC (make-leaf 'C 1))
(define lD (make-leaf 'D 1))
(define lE (make-leaf 'E 1))
(define lF (make-leaf 'F 1))
(define lG (make-leaf 'G 1))
(define lH (make-leaf 'H 1))

(define tGH (make-code-tree lG lH))
(define tEF (make-code-tree lE lF))
(define tEFGH (make-code-tree tEF tGH))
(define tCD (make-code-tree lC lD))
(define tBCD (make-code-tree lB tCD))
(define tBCDEFGH (make-code-tree tBCD tEFGH))
(define tABCDEFGH (make-code-tree lA tBCDEFGH))

(left-branch tABCDEFGH)
(right-branch tABCDEFGH)
(left-branch tBCD)
(right-branch tBCD)

(symbols tABCDEFGH)
(weight tABCDEFGH)
