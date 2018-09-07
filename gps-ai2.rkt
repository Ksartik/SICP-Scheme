#lang racket
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

(define (accumulate proc init lis)
  (if (null? lis)
      init
      (proc (car lis) (accumulate proc init (cdr lis)))))
(define (rm x l)
  (cond ((null? l) '())
        ((eq? (car l) x) (rm x (cdr l)))
        (else (cons (car l) (rm x (cdr l))))))
(define (remove from-list the-list)
  (define (iter re l)
    (if (null? l)
       re
       (iter (rm (car l) re) (cdr l))))
  (iter from-list the-list))

(define (sublist? l1 l2) ; l1 is a subset of l2
  (cond ((null? l1) true)
        ((memq (car l1) l2) (sublist? (cdr l1) l2))
        (else false)))

;Debugging

(define *dbg-ids* '())

;OPERATOR ---
(define (op? op)
  (and (vector? op) (eq? (vector-ref op 0) 'op)))

(define (make-op a . b)
  (let ((pairs (cons a b)))
    (define (opn-search func lis)
      (cond ((null? lis) false)
            ((eq? (caar lis) func) (cdar lis))
            (else (opn-search func (cdr lis)))))
    (λ(func) (opn-search func pairs))))

(define school-ops
  (list
   (make-op '(action drive-son-to-school)
            '(preconds son-at-home car-works)
            '(add-list son-at-school)
            '(del-list son-at-home))
   (make-op '(action shop-installs-battery)
            '(preconds car-needs-battery shop-knows-problem shop-has-money)
            '(add-list car-works))
   (make-op '(action tell-shop-problem)
            '(preconds in-communication-with-shop)
            '(add-list shop-knows-problem))
   (make-op '(action telephone-shop)
            '(preconds know-phone-number)
            '(add-list in-communication-with-shop))
   (make-op '(action look-up-number)
            '(preconds have-phone-book)
            '(add-list know-phone-number))
   (make-op '(action give-shop-money)
            '(preconds have-money)
            '(add-list shop-has-money)
            '(del-list have-money))))

(define banana-ops
  (list
   (make-op '(action climb-on-chair)
            '(preconds chair-at-middle-room at-middle-room on-floor)
            '(add-list at-bananas on-chair)
            '(del-list at-middle-room on-floor))
   (make-op '(action push-chair-from-door-to-middle-room)
            '(preconds chair-at-door at-door)
            '(add-list chair-at-middle-room at-middle-room)
            '(del-list chair-at-door at-door))
   (make-op '(action walk-from-door-to-middle-room)
            '(preconds at-door on-floor)
            '(add-list at-middle-room)
            '(del-1ist at-door))
   (make-op '(action grasp-bananas)
            '(preconds at-bananas empty-handed)
            '(add-list has-bananas)
            '(del-list empty-handed))
   (make-op '(action drop-ball)
            '(preconds has-ball)
            '(add-list empty-handed)
            '(del-list has-ball))
   (make-op '(action eat-bananas)
            '(preconds has-bananas)
            '(add-list empty-handed not-hungry)
            '(del-list has-bananas hungry))))

;GPS ---
(define (gps have need ops)
  (let ((actions-stack (stack))
        (state have)
        (goals need))
    
    (define (appropriate? goal op)
      (memq goal (op 'add-list)))

    (define (apply-op op)
      (let ((del (op 'del-list)))
        (if del
            (begin (for-each (λ(x) (if (memq x del) (set! goals (rm x goals)) true)) goals) (set! state (remove state del)) (set! state (append state (op 'add-list))))
            (set! state (append state (op 'add-list))))))

    (define (achieve goal)
      (define (iter-op ops)
        (cond ((null? ops) false)
              ((appropriate? goal (car ops)) (begin (set! goals (rm goal goals)) (set! goals (append (rm goal ((car ops) 'add-list)) goals)) (set! goals (append ((car ops) 'preconds) goals)) (push actions-stack ((car ops) 'action)) (apply-op (car ops)) true))
              (else (iter-op (cdr ops)))))
      
      (define (iter-state haves)
        (cond ((null? haves) false)
              ((eq? (car haves) goal) (begin (set! goals (rm goal goals)) true))
              (else (iter-state (cdr haves)))))
      
      (or (iter-state state) (iter-op ops)))

    (define (achieve-all)
      (if (null? goals)
          true
          (and (achieve (car goals)) (achieve-all))))


    (let ((final (and (achieve-all) (sublist? need state))))
      (define (print-actions)
        (if (null? (tos actions-stack))
             (if final
                 (display "SOLVED")
                 (display "NIL"))
             (begin (display (string-append "Executing " (symbol->string (car (pop actions-stack))))) (newline) (print-actions))))
      (print-actions))))
      ;(cons state goals))))