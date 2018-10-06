;; ============= ;;
;; Exercise 2.83 ;;
;; ============= ;;

(define (raise x) (apply-generic 'raise x)) 

(define (install-scheme-number-package)
  ;; interface to rest of the system
  ;; ...
  (put 'raise 'integer  
       (lambda (x) (make-rational x 1))) 
  ;; ...
  'done)

(define (install-rational-package)
  ;; internal procedures
  ;; ...
  ;; interface to rest of the system
  ;; ...
  (put 'raise 'rational 
       (lambda (x) (make-real (/ (numer x) (denom x))))) 
  ;; ...
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  ;; ...
  ;; internal procedures
  ;; ...
  ;; interface to rest of the system
  ;; ...
  (put 'raise 'real 
       (lambda (x) (make-from-real-imag x 0))) 
  ;; ...
  'done)
