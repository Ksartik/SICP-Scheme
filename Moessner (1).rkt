#lang racket
(define (partial-sum lis)
    (define (iter res next)
      (cond ((null? next) res)
            ((null? res) (iter (cons (car next) res) (cdr next)))
            (else
             (iter (cons (+ (car next) (car res)) res) (cdr next)))))
    (iter '() lis))

(define (filter-count-mod m lis)
    (define (iter count res next)
      (cond ((null? next) res)
            ((= (remainder count m) 0) (iter (+ count 1) res (cdr next)))
            (else
             (iter (+ count 1) (cons (car next) res) (cdr next)))))
    (reverse (iter 1 '() lis)))

(define (moessner n lis)
    (if (= n 1) lis
        (moessner (- n 1) (partial-sum (filter-count-mod n lis)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))