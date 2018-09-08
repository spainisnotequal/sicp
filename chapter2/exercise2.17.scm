;; ============= ;;
;; Exercise 2.17 ;;
;; ============= ;;


(define (last-pair a-list)
  (list (list-ref a-list (- (length a-list) 1))))

(define odds (list 23 72 149 34))

(last-pair odds)

