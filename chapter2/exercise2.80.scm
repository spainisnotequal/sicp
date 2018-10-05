;; ============= ;;
;; Exercise 2.80 ;;
;; ============= ;;

(define (=zero? x y) (apply-generic '=zero? x y))

(define (install-scheme-number-package)
  ;; interface to rest of the system
  ;; ...
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ;; ...
  'done)

(define (install-rational-package)
  ;; internal procedures
  ;; ...
  (define (=zero? x)
    (= (numer x) 0))
  ;; interface to rest of the system
  ;; ...
  (put '=zero? '(rational) =zero?)
  ;; ...
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  ;; ...
  ;; internal procedures
  ;; ...
  (define (=zero? z)
    (= (magnitude z) 0))
  ;; ...
  ;; interface to rest of the system
  ;; ...
  (put '=zero? '(complex) =zero?)
  ;; ...
  'done)
