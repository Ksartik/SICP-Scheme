#lang racket
(require math/number-theory)
(define (^ x y)
    (expt x y))
(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))
(define (% x y)
  (remainder x y))



(define (stream-map f . streams)
    (if (null? (car streams))
        empty-stream
        (stream-cons
         (apply f (map (λ(x) (stream-first x)) streams))
         (apply stream-map (cons f (map (λ(x) (stream-rest x)) streams))))))

(define (integers-from n)
    (stream-cons n (integers-from (+ n 1))))


(define (sieve s)
    (let ((first (stream-first s)))
      (stream-cons
       first
       (stream-filter
        (λ(x) (not (= (remainder x first) 0)))
        (stream-rest s)))))

(define primes
    (sieve (integers-from 2)))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials (stream-cons 1 (mul-streams (integers-from 2) factorials)))

(define fibs (stream-cons 1 (stream-cons 1 (add-streams fibs (stream-rest fibs)))))

(define (display-stream s n)
  (if (= n 1)
      (printf "~a" (stream-first s))
      (begin (printf "~a~n" (stream-first s)) (display-stream (stream-rest s) (- n 1)))))

(define (show-stream s n)
  (if (= n 1)
      (printf "~a" (stream-first s))
      (begin (printf "~a " (stream-first s)) (show-stream (stream-rest s) (- n 1)))))

(define (partial-sum s)
  (stream-cons (stream-first s) (add-streams (partial-sum s) (stream-rest s))))

(define (scale-stream s x)
  (stream-cons (* (stream-first s) x) (scale-stream (stream-rest s) x)))

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1c (stream-first s1))
               (s2c (stream-first s2)))
           (if(<= s1c s2c)
              (stream-cons s1c (merge (stream-rest s1) s2))
              (stream-cons s2c (merge s1 (stream-rest s2))))))))

(define (merge-hamming s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1c (stream-first s1))
               (s2c (stream-first s2)))
           (cond ((< s1c s2c)
                  (stream-cons s1c (merge-hamming (stream-rest s1) s2)))
                 ((> s1c s2c)
                  (stream-cons s2c (merge-hamming s1 (stream-rest s2))))
                 (else
                  (stream-cons s1c (merge-hamming (stream-rest s1) (stream-rest s2)))))))))

(define (multiples a . b)  ; HAMMING!!!
  (define (iter s n)
    (if (null? (cdr n))
        (scale-stream s (car n))
        (merge-hamming (scale-stream s (car n)) (iter s (cdr n)))))
  (define s (stream-cons 1 (merge-hamming (scale-stream s a) (iter s b))))
  (stream-rest s))

;; Rational number decimal expansion

(define (rational-expand num den)
  (expand num den 10))

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; Continued fraction


;; Power series
(define (integrate-series s)
  (let ((count 0))
    (stream-map (λ(x) (begin (set! count (+ count 1)) (/ x count))) s)))

(define (display-series s n)
  (let ((count 0))
    (define (iter st k)
      (cond ((= k 1)
             (if (= (stream-first st) 0)
                 (printf "")
                 (printf "(~a, ~a) + " (stream-first st) count)))
            ((= (stream-first st) 0) (set! count (+ count 1)) (printf "") (iter (stream-rest st) (- k 1)))
            (else (begin (printf "(~a, ~a) + " (stream-first st) count) (set! count (+ count 1)) (iter (stream-rest st) (- k 1))))))
    (iter s n)))

(define exp-series
    (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define geo-series  ;; 1/(1-x)
  (stream-cons 1 geo-series))

(define geo-alt-series ;; 1/(1+x)
  (stream-map (λ(x) (if (= (remainder x 2) 0) x (- 0 x))) geo-series))

(define (iter count)
    (cond ((= (remainder count 2) 0) (stream-cons 0 (iter (+ count 1))))
          ((= (remainder (/ (- count 1) 2) 2) 0) (stream-cons (/ 1 count) (iter (+ count 1))))
          (else
           (stream-cons (- 0 (/ 1 count)) (iter (+ count 1))))))

(define atan-series
  (iter 0))

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2)) (add-streams (scale-stream (stream-rest s2) (stream-first s1)) (mul-series (stream-rest s1) s2))))


(define (invert-unit-series s) ;; if s is a power series with constant terms as 1, then x such that x.s = 1 si given by <-
  (stream-cons 1 (scale-stream (mul-series (stream-rest s) (invert-unit-series s)) -1)))

(define (div-series s1 s2)
  (if (= (stream-first s2) 0)
      (error "Can't DIVIDE by ZERO!")
      (scale-stream (mul-series s1 (invert-unit-series (scale-stream s2 (/ 1 (stream-first s2))))) (/ 1 (stream-first s2)))))

;; Sorting (Quick Sort) CHECK!!!

(define (sort-stream s)
  (let ((pivot (stream-first s)))
    (let ((l-stream (sort-stream (stream-filter (λ(x) (<= x pivot)) s)))
          (r-stream (sort-stream (stream-filter (λ(x) (> x pivot)) s))))
      (stream-cons l-stream (stream-cons pivot r-stream)))))


;; Formulating iterations as stream processes

(define (sqrt-eval x tolN)
  (let ((s (sqrt-stream x))
        (tol (expt 10 (- 0 tolN))))
    (let ((prev-guess 0))
      (define (check s)
        (let ((guess (stream-first s)))
          (if (< (abs (- guess prev-guess)) tol)
              guess
              (begin (set! prev-guess guess) (check (stream-rest s))))))
      (round-off (check s) tolN))))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (λ(guess) (/ (+ guess (/ x guess)) 2.0)) guesses)))
  guesses)

(define (log-summands t x n)
  (stream-cons (/ (* t x) n)
               (stream-map - (log-summands (* t x) x (+ n 1)))))

(define (log-stream x) (partial-sum (log-summands 1.0 (- x 1) 1))) 

(define (stream-limit s tol)
  (define (iter res)
    (if (< (abs (- (stream-first (stream-rest res)) (stream-first res))) tol)
        (stream-first res)
        (iter (stream-rest res))))
  (iter s))

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream (scale-stream (partial-sum (pi-summands 1)) 4))

; SEQUENCE ACCELERATOR

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (expt (- s2 s1) 2)
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)   ; stream of streams -> s00, s01, s02,... ; s10, s11, s12,... ; ...
  (stream-cons s (make-tableau transform (transform s))))

(define (accelerated-sequence s [transform euler-transform]) ; taking the first element of each transformed streams
  (stream-map stream-first
              (make-tableau transform s)))
;; Big numbers (TRY)

(define (display-num n)
  (if (stream-empty? n)
      (printf "")
      (begin (printf "~a" (stream-first n)) (display-num (stream-rest n)))))

(define (add-bigs n1 n2)
  (define (add-big-stream s1 s2)
    (if (or (stream-empty? s1) (stream-empty? s2))
        (cons 0 empty-stream)
        (let ((s (add-big-stream (stream-rest s1) (stream-rest s2))))
          (let ((carry (car s))
                (stream (cdr s)))
            (let ((res (+ (stream-first s1) (stream-first s2) carry)))
              (cons (floor (/ res 10)) (stream-cons (remainder res 10) stream)))))))
  (let ((ad (add-big-stream n1 n2)))
    (if (= (car ad) 0)
        (cdr ad)
        (stream-cons (car ad) (cdr ad)))))


;; Stream storing pairs -- infinite streams in the sense : S0T0, S0T1, S0T2, ... ; S1T1, S1T2, S1T3, ... ; ... (i.e. only the upper triangular matrix of combinations

(define (interleave s1 s2) ; appending one stream with other so that both reach their respective ends at the same rate.
    (if (stream-empty? s1)
        s2
        (stream-cons (stream-first s1)
                     (interleave s2 (stream-rest s1)))))

(define (unordered-pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (λ(x) (list (stream-first s) x)) (stream-rest t))
    (unordered-pairs (stream-rest s) (stream-rest t)))))

(define int-unord-pairs   
  (unordered-pairs (integers-from 1) (integers-from 1)))   ; (n,n) => comes at the (2^n - 1)th position, (n,k) => (2^n + 2n(k-n) - 3)th position

(define prime-sum-pairs
  (stream-filter (λ(x) (prime? (+ (car x) (cadr x)))) int-unord-pairs))

(define (ordered-pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (λ(x) (list (stream-first s) x)) (stream-rest t))
    (ordered-pairs (stream-rest s) t))))

(define (unordered-triples s t u)
  (stream-cons
   (list (stream-first s) (stream-first t) (stream-first u))
   (interleave
    (stream-map (λ(pair) (cons (stream-first s) pair)) (unordered-pairs t (stream-rest u)))
    (unordered-triples (stream-rest s) (stream-rest t) (stream-rest u)))))

(define pytha-triple
  (stream-filter (λ(x) (= (+ (^ (car x) 2) (^ (cadr x) 2)) (^ (caddr x) 2))) (unordered-triples (integers-from 1) (integers-from 1) (integers-from 1))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1c (stream-first s1))
               (s2c (stream-first s2)))
           (cond ((< (weight s1c) (weight s2c))
                  (stream-cons s1c (merge-weighted (stream-rest s1) s2 weight)))
                 ((> (weight s1c) (weight s2c))
                  (stream-cons s2c (merge-weighted s1 (stream-rest s2) weight)))
                 (else
                  (stream-cons s1c (stream-cons s2c (merge-weighted (stream-rest s1) (stream-rest s2) weight)))))))))

(define (weighted-unord-pairs s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weighted
    (stream-map (λ(x) (list (stream-first s) x)) (stream-rest t))
    (weighted-unord-pairs (stream-rest s) (stream-rest t) weight) weight)))

(define (weighted-ord-pairs s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weighted
    (stream-map (λ(x) (list (stream-first s) x)) (stream-rest t))
    (weighted-ord-pairs (stream-rest s) t weight) weight)))

(define stream-ord-pairs (weighted-ord-pairs (integers-from 1) (integers-from 1) (λ(x) (+ (car x) (cadr x)))))

(define s1 (weighted-unord-pairs (integers-from 1) (integers-from 1) (λ(x) (+ (car x) (cadr x)))))
(define s (stream-filter (λ(x) (not (or (= (remainder x 2) 0) (= (remainder x 3) 0) (= (remainder x 5) 0)))) (integers-from 1)))
(define s2 (weighted-unord-pairs s s (λ(x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x))))))

(define (ramanujan-numbers)
  (define (rama-merge-weighted s1 s2 weight)
    (cond ((stream-empty? s1) empty-stream)
          ((stream-empty? s2) empty-stream)
          (else
           (let ((s1c (stream-first s1))
                 (s2c (stream-first s2)))
             (cond ((< (weight s1c) (weight s2c))
                    (rama-merge-weighted (stream-rest s1) s2 weight))
                   ((> (weight s1c) (weight s2c))
                    (rama-merge-weighted s1 (stream-rest s2) weight))
                   (else
                    (stream-cons (append s1c s2c (list (weight s1c))) (rama-merge-weighted (stream-rest s1) (stream-rest s2) weight))))))))
  (define (rama-weighted-unord-pairs s t weight)
     (rama-merge-weighted
      (stream-map (λ(x) (list (stream-first s) x)) t)
      (rama-weighted-unord-pairs (stream-rest s) (stream-rest t) weight) weight))
  (rama-weighted-unord-pairs (integers-from 1) (integers-from 1) (λ(x) (+ (^ (car x) 3) (^ (cadr x) 3)))))

(define (stream-car s) (stream-first s))
(define (stream-cdr s) (stream-rest s))

(define (Ramanujan s) 
          (define (stream-cadr s) (stream-car (stream-cdr s))) 
          (define (stream-cddr s) (stream-cdr (stream-cdr s))) 
          (let ((scar (stream-car s)) 
                    (scadr (stream-cadr s))) 
                 (if (= (sum-triple scar) (sum-triple scadr))  
                         (stream-cons (list (sum-triple scar) scar scadr) 
                                                  (Ramanujan (stream-cddr s))) 
                         (Ramanujan (stream-cdr s))))) 
(define (triple x) (* x x x)) 
(define (sum-triple x) (+ (triple (car x)) (triple (cadr x))))
(define integers (integers-from 1))
(define (weighted-pairs s t weight) 
   (stream-cons 
    (list (stream-car s) (stream-car t)) 
    (merge-weighted 
     (merge-weighted 
      (stream-map (lambda (x) (list x (stream-car t))) 
                  (stream-cdr s)) 
      (stream-map (lambda (x) (list (stream-car s) x)) 
                  (stream-cdr t)) 
      weight) 
     (weighted-pairs (stream-cdr s) (stream-cdr t) weight) 
     weight)))
(define Ramanujan-numbers 
         (Ramanujan (weighted-pairs integers integers sum-triple)))

;;;

(define (merge-weighted1 s1 s2 weight)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1c (stream-first s1))
               (s2c (stream-first s2)))
           (let ((ws1c (weight s1c))
                 (ws2c (weight s2c)))
             (cond ((< ws1c ws2c)
                    (stream-cons (append s1c (list ws1c)) (merge-weighted1 (stream-rest s1) s2 weight)))
                   ((> ws1c ws2c)
                    (stream-cons (append s2c (list ws2c)) (merge-weighted1 s1 (stream-rest s2) weight)))
                   (else
                    (stream-cons (append s1c (list ws1c)) (stream-cons (append s2c (list ws2c)) (merge-weighted1 (stream-rest s1) (stream-rest s2) weight))))))))))

(define (weighted-unord-pairs1 s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weighted1
    (stream-map (λ(x) (list (stream-first s) x)) (stream-rest t))
    (weighted-unord-pairs1 (stream-rest s) (stream-rest t) weight) weight)))

(define (list-slice l start end)
  (define (rec k x)
    (cond ((or (null? x) (>= k end)) '())
          ((< k start) (rec (+ k 1) (cdr x)))
          (else (cons (car x) (rec (+ k 1) (cdr x))))))
  (rec 0 l))

(define (ramanujan)
  (define (rama-filter s)
    (let ((sc (stream-first s))
          (sn (stream-first (stream-rest s))))
      (if (= (caddr sc) (caddr sn))
          (stream-cons (cons (list (list-slice sc 0 2) (list-slice sn 0 2)) (caddr sc)) (rama-filter (stream-rest s)))
          (rama-filter (stream-rest s)))))
  (rama-filter (stream-rest (weighted-unord-pairs1 (integers-from 1) (integers-from 1) (λ(x) (+ (^ (car x) 3) (^ (cadr x) 3)))))))



;; Streams as signals !!!!! ;;

; Integrators
(define (integral integrand init-val dt)
  (define int
    (stream-cons init-val
                 (add-streams
                  (scale-stream (force integrand) dt)
                  int)))
  int)
;Simpson evaluation of integrals ::
(define (simpson-int f a b)
  (define (int n)
    (let ((d (/ (- b a) n)))
      (stream-cons (simps d) (int (+ n 1)))))
  (define (simps d)
    (define (iter x s)
      (if (= x b)
          (* d (+ (* 0.5 (f x)) s))
          (iter (+ x d) (+ s (f x)))))
    (iter (+ a d) (* 0.5 (f a))))
  (int 1))

(define (RC R C dt)
  (λ(i v0)
    (stream-map (λ(x) (+ x v0)) (add-streams (scale-stream (integral i 0 dt) (/ 1.0 C)) (scale-stream i R)))))

(define (RC-series v0 R C dt)
  (let ((i0 (/ v0 R)))
    (define i
      (stream-cons (/ v0 R) (stream-map (λ(x) (- i0 x)) (scale-stream (integral i 0 dt) (/ 1.0 (* R C))))))
    (stream-rest i)))

(define (deriv f dt) ; f is stream of function values
  (if (stream-empty? f)
      empty-stream
      (stream-cons (/ (- (stream-first (stream-rest f)) (stream-first f)) dt) (deriv (stream-rest f) dt))))

(define (sign-change-detector x1 x2)
  (let ((sgn (* x1 x2)))
    (if (>= sgn 0)
        0
        (if (> x2 0)
            1
            -1))))

(define (make-zero-crossings input-stream last-value)
  (stream-cons
   (sign-change-detector (stream-first input-stream) last-value)
   (make-zero-crossings (stream-rest input-stream)
                        (stream-first input-stream))))

(define (generate-random-stream min max)
  (stream-cons (* (random min max) (random)) (generate-random-stream min max)))
(define sense-data (generate-random-stream -10 10))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings1 (stream-map sign-change-detector sense-data (stream-rest sense-data)))

(define (delayed-integral delayed-integrand init-val dt)
  (define int
    (stream-cons init-val
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt) ; to solve dy/dt = f(y), y(0) = y0 ;;
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (delayed-integral-rec delayed-integrand init dt)
  (stream-cons init
               (let ((integrand (force delayed-integrand)))
                 (if (stream-empty? integrand)
                     empty-stream
                     (delayed-integral-rec (delay (stream-rest integrand))
                                           (+ (* dt (stream-first integrand))
                                              init)
                                           dt)))))

; General characteristic 2nd order differential equation : y'' =  f(y', y) 
(define (solve-2nd f dt y0 dy0)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (delayed-integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; Linear 2nd order de - y'' = ay' + by
(define (solve-2nd-linear a b dt y0 dy0)
  (solve-2nd (λ(x y) (+ (* a x) (* b y))) dt y0 dy0))

(define (RLC R L C dt)
  (λ(vC0 iL0)
    (define iL (solve-2nd-linear (- (* 1.0 (/ R L))) (- (/ 1.0 (* L C))) dt iL0 (- (/ vC0 L 1.0) (* 1.0 (/ R L) iL0))))
    (define vC (scale-stream iL (- (/ 1.0 C))))
    (stream-map (λ(x y) (cons x y)) vC iL)))

(define random-init (random 1 1000000))

(define (rand-update r)
  (random 1 1000000))

(define random-numbers
  (stream-cons random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (stream-cons
   (f (stream-first s) (stream-first (stream-rest s)))
   (map-successive-pairs f (stream-rest (stream-rest s)))))

(define cesaro-stream
  (map-successive-pairs (λ(r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed) 1.0)
     (monte-carlo
      (stream-rest experiment-stream) passed failed)))
  (if (stream-first experiment-stream) ; since the experiment will result in either 1 (passed) or 0 (failed);;; 
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-stream-in-range a b)
  (stream-cons (+ a (* (random) (- b a))) (stream-map (λ(x) (+ a (* (random) (- b a)))) (random-stream-in-range a b))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (stream-ref (monte-carlo (stream-map P (random-stream-in-range x1 x2) (random-stream-in-range y1 y2)) 0 0) trials) (- x2 x1) (- y2 y1)))