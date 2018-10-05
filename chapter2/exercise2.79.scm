;; ============= ;;
;; Exercise 2.79 ;;
;; ============= ;;

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  ;; interface to rest of the system
  ;; ...
  (put 'equ? '(scheme-number scheme-number) =)
  ;; ...
  'done)

(define (install-rational-package)
  ;; internal procedures
  ;; ...
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  ;; interface to rest of the system
  ;; ...
  (put 'equ? '(rational rational) equ?)
  ;; ...
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  ;; ...
  ;; internal procedures
  ;; ...
  (define (equ? z1 z2)
    (and (= (complex-real-part z1) (complex-real-part z2))
         (= (complex-imag-part z1) (complex-imag-part z2))))
  ;; ...
  ;; interface to rest of the system
  ;; ...
  (put 'equ? '(complex complex) equ?)
  ;; ...
  'done)
