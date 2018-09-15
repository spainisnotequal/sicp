;; ============= ;;
;; Exercise 2.55 ;;
;; ============= ;;

'abracadabra
''abracadabra ; (quote abracadabra)
(list 'quote 'abracadabra) ; (quote abracadabra)

(car (list 'quote 'abracadabra))
(car ''abracadabra)
