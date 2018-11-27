;; ======================================== ;;
;; Section 3.3: Modelling with Mutable Data ;;
;; ======================================== ;;

;; ------------------ ;;
;; Share and identity ;;
;; ------------------ ;;

(define x (list 'a 'b))

(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(eq? (car z1) (cdr z1)) ;=> #t (same object)
(eq? (car z2) (cdr z2)) ;=> #f (different objects)

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1
(set-to-wow! z1) ;=> ((wow b) wow b)

z2
(set-to-wow! z2) ;=> ((wow b) a b)

;; ------------------- ;;
;; Representing Queues ;;
;; ------------------- ;;

;; constructor:

(define (make-queue)
  (cons '() '()))

;; selectors:
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;; mutators:
(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; Examples:
(define q (make-queue))
(insert-queue! q 'a) ;=> ((a) a)
(insert-queue! q 'b) ;=> ((a b) b)
(delete-queue! q)    ;=> ((b) b)
(insert-queue! q 'c) ;=> ((b c) c)
(insert-queue! q 'd) ;=> ((b c d) d)
(delete-queue! q)    ;=> ((c d) d)
