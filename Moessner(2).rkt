#lang racket
(define (filter-cond pred-list lis)
    (define (check count next1 next2)
      (cond ((or (null? next1) (null? next2)) '())
            ((= count (car next2)) (cons 0 (check (+ count 1) (cdr next1) (cdr next2))))
            (else
             (cons (car next1) (check (+ count 1) (cdr next1) next2)))))
    (check 1 lis pred-list))

(define (next-list lis)
    (if(null? (cdr lis))
       (cons (car lis) '())
       (if (= (cadr lis) 0)
           (cons 0 (next-list (cdr lis)))
           (cons (car lis) (next-list (cdr lis))))))

(define (reduce-list lis)
    (define (rec next)
      (cond ((null? next) '())
            ((= (car next) 0) (rec (cdr next)))
            (else
             next)))
    (if (= (car lis) 0)
        (reduce-list (cdr lis))
        (cons (car lis) (rec (cdr lis)))))

(define (partial-sum lis)
    (define (iter res next sump)
      (cond ((null? next) res)
            ((null? res) (iter (cons (car next) res) (cdr next) (cons (car next) sump)))
            ((=(car next) 0) (iter (cons 0 res) (cdr next) (cons (+ (car sump) (car next)) sump)))
            (else
             (iter (cons (+ (car next) (car sump)) res) (cdr next) (cons (+ (car sump) (car next))  sump)))))
    (reverse (iter '() lis '())))

(define (moessner lis pred-list)
    (let ((init (partial-sum (filter-cond pred-list lis))))
      (define (rec next)
        (cond ((null? next) '())
              ((= (cadr next) 0)
               (let ((red (reduce-list next)))
                (cons (car red) (rec (cdr red)))))
              (else
               (rec (partial-sum (next-list next))))))
      (rec init)))

;;Additional procs to make the API
(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))))

(define (seq-list seq up-bound)
    (define (rec count)
      (if (> (seq count) up-bound)
          '()
          (cons (seq count) (rec (+ count 1)))))
    (rec 1))

(define (adjoin-set set x)
    (cond ((null? set) (cons x '()))
          ((= (car set) x) set)
          ((> (car set) x) (cons x set))
          (else
           (cons (car set) (adjoin-set (cdr set) x)))))

(define (set lis)
    (define (iter res next)
      (if (null? next)
          res
          (iter (adjoin-set res (car next)) (cdr next))))
    (iter '() lis))

;;Finally
;For usual sequences -
(define (moessner-reduce N seq)
    (moessner (enumerate-interval 1 N) (seq-list seq N)))

;For sequences which may have repeated values -
(define (moessner-reduce-rep N seq)
    (moessner (enumerate-interval 1 N) (set (seq-list seq N))))
;Moessner-reduce for a list -
(define (moessner-reduce-rep-lis N lis)
    (moessner (enumerate-interval 1 N) (set lis)))