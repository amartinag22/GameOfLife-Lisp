(defpackage :gol-life
  (:use :cl)
  (:export :make-grid
           :randomize-grid
           :next-generation
           :grid-width
           :grid-height
           :cell-alive-p))

(in-package :gol-life)

(defun grid-width (grid)
  (array-dimension grid 0))

(defun grid-height (grid)
  (array-dimension grid 1))

(defun make-grid (width height)
  (make-array (list width height) :initial-element 0))

(defun randomize-grid (grid)
  (dotimes (x (grid-width grid))
    (dotimes (y (grid-height grid))
      (setf (aref grid x y) (random 2))))
  grid)

(defun cell-alive-p (grid x y)
  (= (aref grid x y) 1))

(defun count-neighbors (grid x y)
  (let ((count 0))
    (dotimes (dx 3)
      (dotimes (dy 3)
        (let ((nx (+ x (- dx 1)))
              (ny (+ y (- dy 1))))
          (unless (and (= dx 1) (= dy 1))
            (when (and (>= nx 0) (< nx (grid-width grid))
                       (>= ny 0) (< ny (grid-height grid))
                       (cell-alive-p grid nx ny))
              (incf count))))))
    count))

(defun next-cell-state (grid x y)
  (let ((n (count-neighbors grid x y)))
    (if (cell-alive-p grid x y)
        (if (or (= n 2) (= n 3)) 1 0)
        (if (= n 3) 1 0))))

(defun next-generation (grid)
  (let ((new (make-grid (grid-width grid)
                        (grid-height grid))))
    (dotimes (x (grid-width grid))
      (dotimes (y (grid-height grid))
        (setf (aref new x y)
              (next-cell-state grid x y))))
    new))

