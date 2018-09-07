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

(define (and-p x y)
    (and x y))

(define (find-and-remove x l)
  (cond ((null? l) '())
        ((eq? x (car l)) (find-and-remove x (cdr l)))
        (else (cons (car l) (find-and-remove x (cdr l))))))

(define (add-to-list l x)
  (cond ((null? l) (cons x '()))
        ((eq? x (car l)) l)
        (else (cons (car l) (add-to-list (cdr l) x)))))

(define (sublist? l1 l2) ; l1 is a subset of l2
  (cond ((null? l1) true)
        ((memq (car l1) l2) (sublist? (cdr l1) l2))
        (else false)))


;OPERATOR 
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

(define (dbg x)
  (let ((debugb false))
    (define (dispatch m)
      (cond ((eq? m 'debug) (set! debugb true))
            ((eq? m 'undebug) (set! debugb false))
            ((eq? m 'status) debugb)))
    dispatch))

(define db-ind 0)
(define (indent) (set! db-ind (+ db-ind 1)))
(define (de-indent) (set! db-ind (- db-ind 1)))
(define (debug-ind [k db-ind])
  (if (= k 0)
      ""
      (string-append "\t" (debug-ind (- k 1)))))
(define debug-indent
  (λ(x) (string-append (debug-ind) x)))

(define (gps have need ops)
  (let ((actions-stack (stack)))

    (define (search? del-list have-list preconds)
      (define (iter d h hp)
        (cond ((null? h) (list false have-list preconds))
              ((null? d) (list true have-list preconds))
              ((eq? (car d) (car h)) (set! have-list (append hp (cdr h))) (set! preconds (find-and-remove (car d) preconds)) (iter (cdr d) have-list '()))
              (else (iter d (cdr h) (append hp (list (car h)))))))
      (if del-list
          (iter del-list have-list '())
          (list true have-list preconds)))
    
    (define (search-for-need need-list have-list)
      (let ((new-have have-list))
        (define (iter opss lis new)
          (cond ((null? lis) new)
                ((null? opss) false)
                ((memq (car lis) ((car opss) 'add-list)) (if (debug? gps)
                                                             (begin (newline) (display (debug-indent (string-append "Consider: " (symbol->string (car ((car opss) 'action)))))) (indent) (push actions-stack ((car opss) 'action)))
                                                             (push actions-stack ((car opss) 'action)))
                                                         (let ((sdh (search? ((car opss) 'del-list) have-list ((car opss) 'preconds))))
                                                           (if (car sdh)
                                                               (begin (set! new-have (cadr sdh)) (iter ops (cdr lis) (append new (caddr sdh))))
                                                               false)))
                (else (iter (cdr opss) lis new))))
      
        (let ((new-needs (iter ops need-list '())))
          (if new-needs
              (cons new-have new-needs)
              false))))

    (define (need<->have have-list need-list)
      (define (iter nl hl np hp)
        (cond ((null? nl) need-list) ;(cons have-list need-list))
              ((null? hl) (iter (cdr nl) have-list (append np (list (car nl))) '()))
              ((eq? (car nl) (car hl)) (set! need-list (append np (cdr nl))) (iter (cdr nl) have-list np '()))
              (else (iter nl (cdr hl) np (append hp (list (car hl)))))))
      (iter need-list have-list '() '()))

    (define (achieve goal)
      (if (and (debug? gps) (not (null? goal)))
          (begin (newline)(display (debug-indent (string-append "Goal: " (symbol->string (car goal))))))
          (display ""))
      (cond ((null? goal) (if (debug? gps)
                              (begin (newline) (de-indent) (display (debug-indent (string-append "Action: " (symbol->string (car (pop actions-stack)))))) true)
                              true))
            ((null? have) false)
            (else
             (let ((new-pair (search-for-need (need<->have have goal) have)))
               (if new-pair
                   (begin (set! have (car new-pair)) (achieve (cdr new-pair)))
                   false)))))

    (define (achieve-iter need-list)
      (if (null? need-list)
          true
          (if (achieve (list (car need-list)))
              (begin (set! have (add-to-list have (car need-list))) (and (achieve (list (car need-list))) (achieve-iter (cdr need-list))))
              (and (achieve (list (car need-list))) (achieve-iter (cdr need-list))))))
    
    ;(define (iter have-now need-now)
     ; (cond ((null? need-now) true)
      ;      ((null? have-now) false)
       ;     (else
             ;(let ((nh-pair (need<->have have-now need-now)))
        ;     (let ((nh-need (need<->have have-now need-now)))
         ;      (let ((new-pair (search-for-need nh-need have-now)))
          ;       (if new-pair
           ;          (iter (car new-pair) (cdr new-pair))
            ;         false))))))
    
    (let ((final (and (achieve-iter need) (sublist? need have))))
      
      (define (print-actions)
        (if (null? (tos actions-stack))
             (if final
                 (display "SOLVED")
                 (display "NIL"))
             (begin (display (string-append "Executing " (symbol->string (car (pop actions-stack))))) (newline) (print-actions))))

      ;(newline)
      ;(display "START")
      ;(newline)

      ;(print-actions))))
      (list have need))))

(define GPS (dbg gps))
(define (debug)
  (GPS 'debug))
(define (undebug)
  (GPS 'undebug))
(define (debug? x)
  (GPS 'status))