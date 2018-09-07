#lang racket
(require sicp-pict)

(define (sierpinski v1 v2 v3 N)
  (if (= N 0)
      (segments->painter (list (make-segment v1 v2) (make-segment v2 v3) (make-segment v1 v3)))
      (let ((v12 (vector-scale 0.5 (vector-add v1 v2)))
            (v23 (vector-scale 0.5 (vector-add v2 v3)))
            (v31 (vector-scale 0.5 (vector-add v3 v1))))
        (let ((reduced (sierpinski v1 v12 v31 (- N 1))))
          (let ((reduced2 ((transform-painter v12 (make-vect (+ 1.0 (vector-xcor v12)) (vector-ycor v12)) (make-vect (vector-xcor v12) (+ 1.0 (vector-ycor v12)))) reduced))
                (reduced3 ((transform-painter v31 (make-vect (+ 1.0 (vector-xcor v31)) (vector-ycor v31)) (make-vect (vector-xcor v31) (+ 1.0 (vector-ycor v31)))) reduced)))
            (lambda(frame) (reduced frame) (reduced2 frame) (reduced3 frame)))))))

