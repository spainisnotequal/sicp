;; ============= ;;
;; Exercise 2.68 ;;
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

;; ------------------ ;;
;; Decoding procedure ;;
;; ------------------ ;;

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; ------------------------ ;;
;; Set of weighted elements ;;
;; ------------------------ ;;

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

;; ---------------- ;;
;; Encode procedure ;;
;; ---------------- ;;

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


;; ----------------------------------------------------- ;;
;; Sample message and sample tree to test the procedures ;;
;; ----------------------------------------------------- ;;

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; (a d a b b c a)

(encode (decode sample-message sample-tree) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)

(encode-symbol 'f sample-tree) ; symbol not in tree f
