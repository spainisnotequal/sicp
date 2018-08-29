;; ====================== ;;
;; Exercises of Chapter 1 ;;
;; ====================== ;;

;; ------------- ;;
;; Exercise 1.12 ;;
;; ------------- ;;


(define (pascal row column)
  (cond ((= row 0) 0)
	((and (= column 0) (not (= row 1))) 0)
	((> column row) 0)
	((or (= row 1) (= column 1) (= row column)) 1)
	(else (+ (pascal (- row 1) (- column 1))
		 (pascal (- row 1) column)))))

(pascal 0 0)
(pascal 0 1)

(pascal 1 0)
(pascal 1 1)

(pascal 2 0)
(pascal 2 1)
(pascal 2 2)
(pascal 2 3)

(pascal 3 1)
(pascal 3 2)
(pascal 3 3)

(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)

(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)
(pascal 5 5)
