#lang racket
(require sicp-pict)
(define (make-next-point v)
  (let ((vert1 (make-vect 0.25 0.0))
        (vert2 (make-vect 0.75 0.0))
        (vert3 (make-vect 1.0 0.25))
        (vert4 (make-vect 1.0 0.75))
        (vert5 (make-vect 0.75 1.0))
        (vert6 (make-vect 0.25 1.0))
        (vert7 (make-vect 0.0 0.75))
        (vert8 (make-vect 0.0 0.25))
        (rand (random 1 9)))
    (cond ((= rand 1) (vector-scale 0.5 (vector-add v vert1)))
          ((= rand 2) (vector-scale 0.5 (vector-add v vert2)))
          ((= rand 3) (vector-scale 0.5 (vector-add v vert3)))
          ((= rand 4) (vector-scale 0.5 (vector-add v vert4)))
          ((= rand 5) (vector-scale 0.5 (vector-add v vert5)))
          ((= rand 6) (vector-scale 0.5 (vector-add v vert6)))
          ((= rand 7) (vector-scale 0.5 (vector-add v vert7)))
          ((= rand 8) (vector-scale 0.5 (vector-add v vert8))))))

(define random-point-oct
  (λ()
    (let ((x (random))
          (y (random)))
      (cond ((and (< x 0.25) (or (<= y (- 0.25 x)) (<= (- 1 y) (- 0.25 x)))) (random-point-oct))
            ((and (> x 0.75) (or (<= y (- x 0.75)) (<= (- 1 y) (- x 0.75)))) (random-point-oct))
            (else (make-vect x y))))))

(define (chaos-vec-list N)
  (define (iter count vec-list v)
    (if (> count N)
        vec-list
        (iter (+ count 1) (cons v vec-list) (make-next-point v))))
  (iter 1 '() (random-point-oct)))

(define (chaos-game N)
  (paint (segments->painter (map (λ(v) (make-segment (vector-add (make-vect 0.0 0.0) v) (vector-add (make-vect 0.001 0.001) v))) (chaos-vec-list N)))))