;; ============= ;;
;; Exercise 2.37 ;;
;; ============= ;;

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
	    (accumulate proc initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define v (list 1 2 3 4))
(define w (list 4 5 6 6))
(define m (list v w (list 6 7 8 9)))

;; ----------- ;;
;; Dot product ;;
;; ----------- ;;

(define (dot-product v w)
  (map * v w))

(dot-product v w)

;; --------------- ;;
;; Matrix * Vector ;;
;; --------------- ;;

(define (matrix-*-vector m v)
  (map (lambda (w)
	 (accumulate + 0 (dot-product v w)))
       m))

(matrix-*-vector m v)

;; ---------------- ;;
;; Matrix transpose ;;
;; ---------------- ;;

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose m)

;; --------------- ;;
;; Matrix * Matrix ;;
;; --------------- ;;

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rows)
	   (matrix-*-vector cols rows))
	 m)))

(matrix-*-matrix m m)

(define simple-m (list (list 1 2) (list 1 2)))

(matrix-*-matrix simple-m simple-m)
