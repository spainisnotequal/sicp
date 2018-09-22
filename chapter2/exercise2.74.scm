;; ============= ;;
;; Exercise 2.74 ;;
;; ============= ;;

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE_TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; --------- ;;
;; Section a ;;
;; --------- ;;

(define (get-record employee division)
  (attach-tag division ((get 'get-record division) employee)))

;; --------- ;;
;; Section b ;;
;; --------- ;;

(define (get-salary employee-record)
  ((get 'get-salary (type-tag employee-record)) (contents employee-record)))

;; --------- ;;
;; Section c ;;
;; --------- ;;

(define (find-employee-record employee divisions-list)
  (if (null? divisions-list)
      false
      (let ((record (get-record employee (car divisions-list))))
	(if record ; "get-record" returns "false" is not record is found
	    record
	    (find-employee-record employee (cdr divisions-list))))))


;; --------- ;;
;; Section d ;;
;; --------- ;;

;; To incorporate the personel information of a new company, we should write the corresponding "get-record" and "get-salary" for that new company according to their particular way of structuring the personnel information.
