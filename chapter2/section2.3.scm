;; ========================== ;;
;; Section 2.3: Symbolic Data ;;
;; ========================== ;;


;; --------------- ;;
;; 2.3.1 Quotation ;;
;; --------------- ;;

(define a 1)
(define b 2)
(list a b)
(list 'a 'b) ; (a b)

(car '(a b c)) ; a
(cdr '(a b c)) ; (b c)

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(memq 'apple '(pear banana prune)) ; #f
(memq 'apple '(pear apple banana prune)) ; (apple banana prune)
(memq 'apple '(pear (peach apple) banana prune)) ; #f
(memq 'apple '(pear (apple peach) banana prune)) ; #f
(memq 'apple '(pear (apple peach) banana apple prune)) ; (apple prune)
