;;Transformati o lista imbricata intr-o lista simpla.
;;Exemplu: '(1 (2 3 (4 5) 6) 7 (8 9)) ⇒ '(1 2 3 4 5 6 7 8 9)

(define flatten (lambda (l) (if (empty? l) '() 
                      (if (list? (car l)) ( append (flatten (car l) ) (flatten (cdr l)))
                           (cons (car l) (flatten (cdr l)))))))

;;Transformati o lista de elemente consecutive identice, in liste de liste, fiecare continand exact elementele identice.
;;Exemplu: '(1 1 1 1 1 2 2 2 3 4 5) ⇒ '( (1 1 1 1 1) (2 2 2) (3) (4) (5) ).
(define consecutiveF( lambda (l) (if (empty? l) '()
                                   (cons (filter  (lambda (x) (eq? x (car l))) l ) (consecutiveF (filter (lambda (x) (not (eq? x (car l)))) (cdr l)))))))
(define consecutive( lambda (h l) (if (empty? l) (cons h '())
                                     (if (empty? h) (consecutive (cons (car l) h) (cdr l))
                                         (if (eq? (car h) (car l)) (consecutive (cons (car l) h) (cdr l))
                                             (cons h (consecutive '()  l)))))))
;;Pentru o lista ce poate contine doar elemente consecutive identice, construiti o lista de <element,nr_aparitii>.
;;Exemplu: '(1 1 1 1 1 2 2 2 3 4 5) ⇒ '( (1 4) (2 3) (3 1) (4 1) (5 1) ).
(define ident(lambda (l) (if (empty? l) '()
                             (cons (cons (car l) (length (filter  (lambda (x) (eq? x (car l))) l ))) (ident (filter (lambda (x) (not (eq? x (car l)))) l))))))
;;Pentru o lista arbitrara, construiti o lista de elemente: <element,nr_aparitii>.
;;Exemplu: '(1 2 3 1 4 2 2 5) ⇒ '( (1 4) (2 3) (3 1) (4 1) (5 1)) ).
;;functioneaza cea de sus
;;Implementati rotatia cu n pozitii la stanga/dreapta a unei liste:
;;Exemplu: o rotatie de 3 pozitii la stanga pentru '(1 2 3 4 5 6 7) produce '(4 5 6 7 1 2 3)
(define take( lambda (n l) (if (eq? n 0) '()
                               ( cons (car l) (take (- n 1) (cdr l))))))
(define drop (lambda (n l) ( if (eq? n 0) l
                               (drop (- n 1) (cdr l)))))
(define slice (lambda (n l) (append (drop n l) (take n l))))

;;Implementati functia ce verifica daca un element apare intr-o lista data.
;;Exemplu: (contains 2 '(1 2 3 4)) => #t
;;Atentie: testati implementarea si pe liste imbricate: (contains `(1 2) `(1 ,`(1 2) ,`(2 3))) => #t
(define cont(lambda (e l) (if (empty? l) #f
                                  (if (equal? e (car l)) #t
                                      (cont e (cdr l))))))

;; Duplicati aparitiile elementelor unei liste de un numar dat de ori.
;;Exemplu: (duplicate '(a b c) 3) => '(a a a b b b c c c)
(define rep (lambda (n x) (if ( eq? x 0) '() (cons n ( rep n (- x 1))))))
     
(define duplicate (lambda (l x) (if ( empty? l) '()
                                    (append (rep (car l) x) (duplicate (cdr l) x)))))

;;Dandu-se doi indecsi: l, r, si o lista list, extrageti sublista din list dintre cei doi indecsi. 
;;Exemplu: (sublist 2 4 '(a b c d e f g)) => '(b c d)
(define sublist( lambda (l r L) (if (eq? r 0) '()
                                    (if (eq? l 1) (cons (car L) (sublist l (- r 1) (cdr L)))
                                        (sublist (- l 1) (- r 1) (cdr L))))))
;;Inserati un element la o anumita pozitie intr-o lista.
;;Exemplu: (insert_at e 2 '(a b c d f)) => '(a e b c d f)
(define insert_at(lambda (e p l) (if (eq? p 1) (cons e l)
                                     ( cons (car l) ( insert_at e (- p 1) (cdr l))))))

;;Determinati daca un numar dat este prim.
;;Exemplu: (prime? 7) => #t
;;(prime? 8) => #f

(define devides (lambda (y x) (eq? (modulo x y) 0))) 
(define helper (lambda (n d) ( if (eq? d 1) #f
                                  (or (devides d n) (helper n (- d 1))))))
(define prime? (lambda (n) (not (helper n (- n 1)))))

;;Dandu-se un interval definit prin capetele acestuia, construiti o lista cu toate numerele prime din acel interval.
;;Exemplu: (primes 3 12) => '(3 5 7 11)
(define primes (lambda (l r) ( if (eq? l r) '()
                                  (if (prime? l) (cons l (primes (+ l 1) r))
                                      (primes (+ l 1) r)))))