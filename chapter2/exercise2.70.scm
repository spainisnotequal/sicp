;; ============= ;;
;; Exercise 2.70 ;;
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

;; ------ ;;
;; Encode ;;
;; ------ ;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
	    (right (right-branch tree)))
	(cond ((symbol-in-tree? symbol left)
	       (cons 0 (encode-symbol symbol left)))
	      ((symbol-in-tree? symbol right)
	       (cons 1 (encode-symbol symbol right)))
	      (else (error "symbol not in tree" symbol))))))

(define (symbol-in-tree? symbol tree)
  (member symbol (symbols tree)))


;; -------- ;;
;; Exercise ;;
;; -------- ;;


(define alphabet '((a 2) (boom 1) (get 2) (job 2) (sha 3) (na 16) (wah 1) (yip 9)))

(define lyrics '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(encode lyrics (generate-huffman-tree alphabet))
(length (encode lyrics (generate-huffman-tree alphabet)))
