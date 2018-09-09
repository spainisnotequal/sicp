;; ============= ;;
;; Exercise 2.29 ;;
;; ============= ;;

;; ----------- ;;
;; Section (a) ;;
;; ----------- ;;

;; Mobile

(define (make-mobile left right)
  (list left right))

(define (left-branch a-mobile)
  (car a-mobile))

(define (right-branch a-mobile)
  (car (cdr a-mobile)))


;; Branch

(define (make-branch length structure)
  (list length structure))

(define (branch-length a-branch)
  (car a-branch))

(define (branch-structure a-branch)
  (car (cdr a-branch)))

;; ----------- ;;
;; Section (b) ;;
;; ----------- ;;

;; Weight procedures

(define (branch-weight a-branch)
  (let ((structure (branch-structure a-branch)))
    (if (not (pair? structure))
	structure
	(total-weight structure))))

(define (total-weight a-mobile)
   (+ (branch-weight (left-branch a-mobile))
      (branch-weight (right-branch a-mobile))))

;; Tests:

(define branch1 (make-branch 100 2))
(define branch2 (make-branch 100 3))

(define mobile1 (make-mobile branch1 branch2))

(define branch3 (make-branch 100 mobile1))
(define branch4 (make-branch 100 5))

(define mobile2 (make-mobile branch3 branch4))

(branch-weight branch1)
(branch-weight branch2)

(total-weight mobile1)

(branch-weight branch3)
(branch-weight branch4)

(total-weight mobile2)


;; ----------- ;;
;; Section (c) ;;
;; ----------- ;;

(define (balanced? a-mobile)
  (if (not (pair? a-mobile))
      #t
      (and (= (torque (left-branch a-mobile))
	      (torque (right-branch a-mobile)))
           (balanced? (branch-structure (left-branch a-mobile)))
           (balanced? (branch-structure (right-branch a-mobile))))))

(define (torque a-branch)
  (* (branch-length a-branch) (branch-weight a-branch)))


;; Tets:

(define b1 (make-branch 2 10))
(define b2 (make-branch 2 5))
(define m1 (make-mobile b2 b2))
(define b3 (make-branch 2 m1))

(define balanced-mobile (make-mobile b1 b3))
(define unbalanced-mobile (make-mobile b2 b3))

(balanced? balanced-mobile)
(balanced? unbalanced-mobile)


;; ----------- ;;
;; Section (d) ;;
;; ----------- ;;


;; Mobile

(define (make-mobile left right)
  (cons left right))

;; left-branch doesn't change
(define (left-branch a-mobile)
  (car a-mobile))

;; right-branch changes
(define (right-branch a-mobile)
  (cdr a-mobile))


;; Branch

(define (make-branch length structure)
  (cons length structure))

;; branch-lenght doesn't change
(define (branch-length a-branch)
  (car a-branch))

;; branch-structure changes
(define (branch-structure a-branch)
  (cdr a-branch))

;; Tests:

(define branch1 (make-branch 100 2))
(define branch2 (make-branch 100 3))
(define mobile1 (make-mobile branch1 branch2))

(branch-length branch1)
(branch-structure branch1)

(left-branch mobile1)
(right-branch mobile1)
