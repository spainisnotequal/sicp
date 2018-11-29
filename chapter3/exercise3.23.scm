;; ============= ;;
;; Exercise 3.23 ;;
;; ============= ;;

;; constructor:
(define (make-deque)
  (cons '() '()))

;; predicate:
(define (empty-deque? deque)
  (null? (front-ptr deque)))

;; selectors:
(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (caar (rear-ptr deque))))

;; mutators:
(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! new-pair (front-ptr deque))
           (set-cdr! (car (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-cdr! (car new-pair) (rear-ptr deque))
           (set-rear-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "DELETE! called with an empty deque" deque)
      (if (null? (cdr (front-ptr deque)))
          (set-front-ptr! deque '())
          (set-rear-ptr! deque (cdar (rear-ptr deque))))))


(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "DELETE! called with an empty deque" deque)
      (if (null? (cdr (front-ptr deque)))
          (set-front-ptr! deque '())
          (set-front-ptr! deque (cdr (front-ptr deque))))))

(define (print-deque deque) 
   (display (map car 
                 (front-ptr deque)))
   (newline))

;; Tests:
;; -----

;; constructor:
(define dq (make-deque))
(print-deque dq) ;=> ()
(front-deque dq) ;=> FRONT called with an empty deque (())
(rear-deque dq) ;=> REAR called with an empty deque (())
(empty-deque? dq) ;=> #t

;; insertion and deletions:
(front-insert-deque! dq 'a)
(empty-deque? dq) ;=> #f
(print-deque dq) ;=> (a)
(front-delete-deque! dq)
(empty-deque? dq) ;=> #t

(front-insert-deque! dq 'a)
(empty-deque? dq) ;=> #f
(print-deque dq) ;=> (a)
(rear-delete-deque! dq)
(empty-deque? dq) ;=> #t
(rear-delete-deque! dq) ;=> DELETE! called with an empty deque (() (a))
(empty-deque? dq) ;=> #t

;; insertion and deletions:
(rear-insert-deque! dq 'a)
(print-deque dq) ;=> (a)
(front-insert-deque! dq 'b)
(print-deque dq) ;=> (b a) 
(rear-insert-deque! dq 'c)
(print-deque dq) ;=> (b a c)
(rear-insert-deque! dq 'd)
(print-deque dq) ;=> (b a c d)

(front-deque dq) ;=> b
(rear-deque dq) ;=> d
