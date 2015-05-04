(define sempty '())

(define-syntax scons
  (syntax-rules ()
    ((scons head tail)
     (cons head (delay tail)))))

(define scar car)

(define (scdr s) (force (cdr s)))

(define ones (scons 1 ones))

(define (snull? s)
  (equal? s sempty))

(define (stake n s)
  (if (or (zero? n) (snull? s))
      sempty
      (cons (scar s) (stake (- n 1) (scdr s)))))

(define (szipWith f s1 s2)
  (if (or (snull? s1) (snull? s2))
      sempty
      (scons (f (scar s1) (scar s2)) (szipWith f (scdr s1) (scdr s2)))))

(define twos (szipWith + ones ones))

(define nats
  (scons 1 (szipWith + ones nats)))

(define evans (scons 0 (szipWith + nats nats)))

(define fibo
  (scons 0 (scons 1 (szipWith + (scdr fibo) fibo))))

(stake 10 ones)
(stake 10 twos)
(stake 10 nats)
(stake 10 evans)
(stake 10 fibo)