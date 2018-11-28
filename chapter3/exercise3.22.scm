;; ============= ;;
;; Exercise 3.22 ;;
;; ============= ;;

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'empty-queue?)  (empty-queue?))
            ((eq? m 'front-queue)   (front-queue))
            (else (error "Unknown action -- MAKE-QUEUE" m))))
    dispatch))

(define (empty-queue? q) (q 'empty-queue?))
(define (front-queue q) (q 'front-queue))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) (q 'delete-queue!))


;; Tests:
(define q (make-queue))
(empty-queue? q)     ;=> #t
(front-queue q)      ;=> FRONT called with an empty queue
(insert-queue! q 'a) ;=> (a)
(empty-queue? q)     ;=> #f
(front-queue q)      ;=> a
(insert-queue! q 'b) ;=> (a b)
(front-queue q)      ;=> a
(delete-queue! q)    ;=> (b)
(front-queue q)      ;=> b
(delete-queue! q)    ;=> ()
(front-queue q)      ;=> FRONT called with an empty queue
