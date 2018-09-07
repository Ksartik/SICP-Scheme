#lang racket
(require sicp-pict)
(define (triangle-seg-list)
  (let ((vert1 (make-vect 0.1 (- 1 (* (/ (sqrt 3) 2.0) 0.79))))
        (vert2 (make-vect 0.5 0.9))
        (vert3 (make-vect 0.9 (- 1 (* (/ (sqrt 3) 2.0) 0.79)))))
    (list (make-segment vert1 vert2) (make-segment vert2 vert3) (make-segment vert3 vert1))))

(define (square x) (* x x))

(define (mag-vect v)
  (sqrt (+ (square (vector-xcor v)) (square (vector-ycor v)))))

(define (mag seg)
  (sqrt (+ (square (- (vector-xcor (segment-start seg)) (vector-xcor (segment-end seg)))) (square (- (vector-ycor (segment-start seg)) (vector-ycor (segment-end seg)))))))

(define (unit v)
  (vector-scale (/ 1.0 (mag-vect v)) v))

(define (normal-unit v)
  (unit (make-vect (- 0.0 (vector-ycor v)) (vector-xcor v))))

(define (rotate vector theta)
  (make-vect (- (* (vector-xcor vector) (cos theta)) (* (vector-ycor vector) (sin theta))) (+ (* (vector-ycor vector) (cos theta)) (* (vector-xcor vector) (sin theta)))))

(define (koch-transform seg)
  (let ((trisect-start (vector-scale (/ 1.0 3.0) (vector-add (segment-end seg) (vector-scale 2 (segment-start seg)))))
        (trisect-end (vector-scale (/ 1.0 3.0) (vector-add (segment-start seg) (vector-scale 2 (segment-end seg)))))
        (a (/ (mag seg) 3.0)))
    (let ((along-vect (unit (vector-sub trisect-start (segment-start seg)))))
      (let ((new-vect (vector-add trisect-start (vector-add (vector-scale (/ a 2.0) along-vect) (vector-scale (/ (* a (sqrt 3)) 2.0)  (normal-unit along-vect))))))
        (list (make-segment (segment-start seg) trisect-start) (make-segment trisect-start new-vect) (make-segment new-vect trisect-end) (make-segment trisect-end (segment-end seg)))))))


(define (koch-reduce segment-list)
  (if (null? segment-list)
      '()
      (append (koch-transform (car segment-list)) (koch-reduce (cdr segment-list)))))


(define (make-koch n)
  (define (iter count snowflake)
    (if (> count n)
        (paint-hi-res (segments->painter snowflake))
        (iter (+ count 1) (koch-reduce snowflake))))
  (iter 1 (triangle-seg-list)))