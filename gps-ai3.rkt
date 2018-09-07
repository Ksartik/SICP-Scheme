#lang racket
(require racket/format)
(define (stack)
    (let ((st '()))
      (define (dispatch m)
        (cond ((eq? m 'push) (λ(c) (set! st (cons c st))))
              ((eq? m 'pop) (if (null? st)
                                (error "UNDERFLOW")
                                (let ((c (car st)))
                                  (begin (set! st (cdr st)) c))))
              ((eq? m 'length) (length st))
              ((eq? m 'tos) (if (null? st)
                                '()
                                (car st)))))
      dispatch))
(define (push st c)
  ((st 'push) c))
(define (pop st)
  (st 'pop))
(define (stack-length st)
  (st 'length))
(define (tos st)
  (st 'tos))

(define (memqual x l)
  (cond ((null? l) false)
        ((equal? x (car l)) l)
        (else (memqual x (cdr l)))))

(define (accumulate proc init lis)
  (if (null? lis)
      init
      (proc (car lis) (accumulate proc init (cdr lis)))))

(define (filter p l)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (any test l)
  (if (null? l)
      false
      (if (test (car l))
          true
          (any test (cdr l)))))

(define (some test l)
  (if (null? l)
      false
      (let ((c (test (car l))))
        (if c
            c
            (some test (cdr l))))))

(define (every test l)
  (if (null? l)
      true
      (and (test (car l)) (every test (cdr l)))))

(define (set-difference l1 l2) ; elements that are in l1 and not in l2
  (cond ((null? l1) '())
        ((memq (car l1) l2) (cons (car l1) (set-difference (cdr l1) l2)))
        (else (set-difference (cdr l1) l2))))

(define (union l1 l2)
  (append (set-difference l1 l2) l2))

(define (remove-if p l)
  (filter (λ(x) (not (p x))) l))

(define (find-all-if p l)
  (filter (λ(x) (p x)) l))

(define (count-if p l)
  (cond ((null? l) 0)
        ((p (car l)) (+ 1 (count-if p (cdr l))))
        (else (count-if p (cdr l)))))
      
(define (rm x l)
  (cond ((null? l) '())
        ((equal? (car l) x) (cdr l))
        (else (cons (car l) (rm x (cdr l))))))
(define (remove from-list the-list)
  (define (iter re l)
    (if (null? l)
       re
       (iter (rm (car l) re) (cdr l))))
  (iter from-list the-list))

(define (sublist? l1 l2) ; l1 is a subset of l2
  (cond ((null? l1) true)
        ((memqual (car l1) l2) (sublist? (cdr l1) l2))
        (else false)))

(define (mappend f l)
  (if (null? l)
      '()
      (append (f (car l)) (mappend f (cdr l)))))

(define (fringe-symbol->string s)
  (if (symbol? s)
      (symbol->string s)
      (fringe-symbol->string (car s))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (list-add-ref l . a)
    (define (iter li b)
      (cond ((null? li) '())
            ((equal? (car li) '%) (cons (car b) (iter (cdr li) (cdr b))))
            (else (cons (car li) (iter (cdr li) b)))))
    (iter l a))

;DEBUG --
(define this-deb '())
(define (add-debug x)
  (set! this-deb (cons (list x false) this-deb)))
(define (debug-val x)
  (let ((val (some (λ(y) (if (eq? (car y) x) y false)) this-deb)))
    (if val
        (cadr val)
        (error "NOT FOUND" x))))
(define (debug x)
  (set! this-deb (map (λ(y) (if (eq? (car y) x) (list (car y) true) y)) this-deb)))
(define (undebug x)
  (set! this-deb (map (λ(y) (if (eq? (car y) x) (list (car y) false) y)) this-deb)))
(define (indent-print times)
  (if (= times 0)
      (display "")
      (begin (display "\t") (indent-print (- times 1)))))
(define (debug-indent x ind-len str)
  (if (debug-val x)
      (begin (indent-print ind-len) (display str) (newline))
      (display "")))
;OPERATOR ---
(define (op? op)
  (and (vector? op) (eq? (vector-ref op 0) 'op)))

(define (make-op a . b)
  (let ((pairs (cons a b)))
    (define (opn-search func lis)
      (cond ((null? lis) false)
            ((eq? (caar lis) func) (cdar lis))
            (else (opn-search func (cdr lis)))))
    (λ(func) (if (eq? func 'give-me-pairs) pairs (opn-search func pairs)))))

(define (starts-with l t)
  (and (pair? l) (eq? (car l) t)))

(define (executing? x)
  (starts-with x 'executing))

(define (convert-op op)
  (apply make-op (map (λ(x) (if (eq? (car x) 'add-list) (append '(add-list) (append (list (list 'executing (car (op 'action)))) (cdr x))) x)) (op 'give-me-pairs))))
;(define (convert-op op)
;  (make-op (op (append (list (list 'executing (car (op 'action)))) (op 'add-list))))

(define (op a . b)
  (convert-op (apply make-op (append (list a) b))))

(define school-ops
  (list
   (op '(action drive-son-to-school)
       '(preconds son-at-home car-works)
       '(add-list son-at-school)
       '(del-list son-at-home))
   (op '(action shop-installs-battery)
       '(preconds car-needs-battery shop-knows-problem shop-has-money)
       '(add-list car-works))
   (op '(action tell-shop-problem)
       '(preconds in-communication-with-shop)
       '(add-list shop-knows-problem))
   (op '(action telephone-shop)
       '(preconds know-phone-number)
       '(add-list in-communication-with-shop))
   (op '(action look-up-number)
       '(preconds have-phone-book)
       '(add-list know-phone-number))
   (op '(action give-shop-money)
       '(preconds have-money)
       '(add-list shop-has-money)
       '(del-list have-money))))

(define banana-ops
  (list
   (op '(action climb-on-chair)
       '(preconds chair-at-middle-room at-middle-room on-floor)
       '(add-list at-bananas on-chair)
       '(del-list at-middle-room on-floor))
   (op '(action push-chair-from-door-to-middle-room)
       '(preconds chair-at-door at-door)
       '(add-list chair-at-middle-room at-middle-room)
       '(del-list chair-at-door at-door))
   (op '(action walk-from-door-to-middle-room)
       '(preconds at-door on-floor)
       '(add-list at-middle-room)
       '(del-1ist at-door))
   (op '(action grasp-bananas)
       '(preconds at-bananas empty-handed)
       '(add-list has-bananas)
       '(del-list empty-handed))
   (op '(action drop-ball)
       '(preconds has-ball)
       '(add-list empty-handed)
       '(del-list has-ball))
   (op '(action eat-bananas)
       '(preconds has-bananas)
       '(add-list empty-handed not-hungry)
       '(del-list has-bananas hungry))))

(define (make-maze-op here there)
  ;(op (list 'action (string->symbol (string-append (string-append "move from " (~a here)) (string-append " to " (~a there)))))
   ;   (list 'preconds (string->symbol (string-append "at " (~a here))))
    ;  (list 'add-list (string->symbol (string-append "at " (~a there))))
     ; (list 'del-list (string->symbol (string-append "at " (~a here))))))
  (op (list 'action (append (append (append '(move from) (list here)) '(to)) (list there)))
      (list 'preconds (append '(at) (list here)))
      (list 'add-list (append '(at) (list there)))
      (list 'del-list (append '(at) (list here)))))

(define (make-maze-ops pair)
  (list (make-maze-op (car pair) (cadr pair))
        (make-maze-op (cadr pair) (car pair))))

(define maze-ops1
  (mappend make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
             (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
             (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(define (nbrs k n)
    (let ((r (remainder k n)))
      (cond ((= r 0) (find-all-if (λ(x) (and (> x 0) (<= x (* n n)))) (list (- k 1) (- k n) (+ k n))))
            ((= r 1) (find-all-if (λ(x) (and (> x 0) (<= x (* n n)))) (list (+ k 1) (- k n) (+ k n))))
            (else (find-all-if (λ(x) (and (> x 0) (<= x (* n n)))) (list (- k 1) (+ k 1) (- k n) (+ k n)))))))

(define (grid-adjacent n)
    (mappend (λ(x) (map (λ(y) (list x y)) (nbrs x n))) (enumerate-interval 1 (* n n))))
(define (make-maze n barriers)
  (remove (grid-adjacent n) (mappend (λ(x) (list x (reverse x))) barriers)))

(define maze-ops2
  (mappend make-maze-ops
           (make-maze 6 '((2 3) (2 8) (8 9) (7 13) (14 20) (19 25) (25 31) (26 27) (21 27) (27 28) (28 34) (28 22) (21 22) (22 16) (16 17) (17 11) (11 10) (11 5) (4 10) (18 24) (24 30) (35 36)))))

(define (make-blocks-ops item from to)
  (op (list 'action (list-add-ref '(move % from % to %) item from to))
      (list 'preconds (list-add-ref '(space on %) to) (list-add-ref '(% on %) item from) (list-add-ref '(space on %) item))
      (list 'add-list (list-add-ref '(space on %) from) (list-add-ref '(% on %) item to))
      (list 'del-list (list-add-ref '(space on %) to) (list-add-ref '(% on %) item from))))

(define blocks-ops1
  (map (λ(x) (apply make-blocks-ops x))
           '((a b c) (a b table) (a c b) (a c table) (a table b) (a table c)
             (b a c) (b a table) (b c a) (b c table) (b table a) (b table c)
             (c a b) (c a table) (c b a) (c b table) (c table a) (c table b))))
  

(define (action? x ops)
  (if (pair? x)
      (if (equal? x '(start))
          true
          (any (λ(y) (equal? (cadr x) (car (y 'action)))) ops))
      false))          
 
;GPS ---
(define (gps state goals ops)
  
  (define (appropriate? goal op)
    (memqual goal (op 'add-list)))

  (define (appropriate-ops goal state)
    (sort (find-all-if (λ(x) (appropriate? goal x)) ops) < #:key (λ(op)
                                                                  (count-if (λ(precond)
                                                                              (not (memqual precond state)))
                                                                            (op 'preconds)))))

  (define (apply-op state goal op goal-stack)
    ;(debug-indent 'gps (length goal-stack) (string-append "Consider: " (fringe-symbol->string (op 'action))))
    (debug-indent 'gps (length goal-stack) (string-append "Consider: " (~a (op 'action))))
    (let ((state2 (achieve-all state (op 'preconds)
                               (cons goal goal-stack))))
      (if (and state2 (not (null? state2)))
          ;(begin (debug-indent 'gps (length goal-stack) (string-append "Action: " (fringe-symbol->string (op 'action))))
           (begin (debug-indent 'gps (length goal-stack) (string-append "Action: " (~a (op 'action))))
                  (append (remove-if (λ(x)
                               (if (op 'del-list)
                                   (memqual x (op 'del-list))
                                   false))
                             state2)
                  (op 'add-list)))
          false)))
    
  (define (achieve state goal goal-stack)
    ;(debug-indent 'gps (length goal-stack) (string-append "Goal: " (fringe-symbol->string goal)))
    (debug-indent 'gps (length goal-stack) (string-append "Goal: " (~a goal)))
    (cond ((memqual goal state) state)
          ((memqual goal goal-stack) false)
          (else
           (some (λ(op) (apply-op state goal op goal-stack)) (appropriate-ops goal state)))))

  (define (achieve-all state goals goal-stack)
    (let ((current-state state))
      (if (and (every (λ(g)
                        (let ((ac (achieve current-state g goal-stack)))
                          (if ac
                              (begin (set! current-state ac) ac)
                              (begin (set! current-state '()) false))))
                      goals)
               (sublist? goals current-state))
          current-state
          '())))

  (define (print-actions a)
    (if (null? (cdr a))
        (display (car a))
        (begin (display (car a)) (newline) (print-actions (cdr a)))))
  (let ((final (find-all-if (λ(x) (action? x ops)) (achieve-all (cons '(start) state) goals '()))))
    (if (null? final)
        (display "NIL")
        (begin (display "(") (newline) (print-actions final) (newline) (display ")")))))

(add-debug 'gps)