(defpackage :gol-ui
  (:use :cl :sdl2)
  (:import-from :gol-life
                :make-grid
                :randomize-grid
                :next-generation
                :grid-width
                :grid-height
                :cell-alive-p)
  (:export :run))

(in-package :gol-ui)

;;; CONFIG
(defparameter *cell-size* 10)
(defparameter *grid-width* 80)
(defparameter *grid-height* 60)

(defparameter *default-delay* 200)
(defparameter *delay* *default-delay*)
(defparameter *running* t)

(defparameter *generation* 0)
(defparameter *elapsed-ms* 0)
(defparameter *last-tick* 0)

(defparameter *mouse-down* nil)

;;; DRAWING
(defun draw-grid (renderer grid)
  (dotimes (x (grid-width grid))
    (dotimes (y (grid-height grid))
      (when (cell-alive-p grid x y)
        (sdl2:render-fill-rect
         renderer
         (sdl2:make-rect
          (* x *cell-size*)
          (* y *cell-size*)
          *cell-size*
          *cell-size*))))))

(defun draw-cell (grid mx my)
  (let ((x (floor mx *cell-size*))
        (y (floor my *cell-size*)))
    (when (and (>= x 0) (< x (grid-width grid))
               (>= y 0) (< y (grid-height grid)))
      (setf (aref grid x y) 1))))

;;; TIME / TITLE
(defun format-time (ms)
  (let* ((sec (floor ms 1000))
         (h (floor sec 3600))
         (m (floor (mod sec 3600) 60))
         (s (mod sec 60)))
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defun update-title (window)
  (sdl2:set-window-title
   window
   (format nil "Game of Life | Gen: ~D | Time: ~A | Delay: ~D ms | ~A"
           *generation*
           (format-time *elapsed-ms*)
           *delay*
           (if *running* "RUNNING" "PAUSED"))))

;;; SEEDS 1–5
(defun clear-grid (grid)
  (dotimes (x (grid-width grid))
    (dotimes (y (grid-height grid))
      (setf (aref grid x y) 0))))

(defun set-cells (grid cells)
  (dolist (c cells)
    (setf (aref grid (first c) (second c)) 1)))

(defun center ()
  (values (floor *grid-width* 2)
          (floor *grid-height* 2)))

(defun apply-seed (grid seed)
  (clear-grid grid)
  (multiple-value-bind (cx cy) (center)
    (case seed
      (1 (randomize-grid grid))
      (2 (set-cells grid `((,cx ,cy) (,cx ,(+ cy 1)) (,cx ,(- cy 1)))))
      (3 (set-cells grid `((,cx ,cy) (,cx ,(+ cy 1)) (,cx ,(+ cy 2))
                           (,(+ cx 1) ,(+ cy 1)) (,(+ cx 1) ,(+ cy 2)) (,(+ cx 1) ,(+ cy 3)))))
      (4 (set-cells grid `((,cx ,cy) (,cx ,(+ cy 1)) (,(+ cx 1) ,cy) (,(+ cx 1) ,(+ cy 1))
                           (,(+ cx 2) ,(+ cy 2)) (,(+ cx 2) ,(+ cy 3))
                           (,(+ cx 3) ,(+ cy 2)) (,(+ cx 3) ,(+ cy 3)))))
      (5 (set-cells grid `((,cx ,cy) (,(+ cx 1) ,(+ cy 1)) (,(+ cx 1) ,(+ cy 2))
                           (,cx ,(+ cy 2)) (,(+ cx -1) ,(+ cy 2)))))))

  (setf *generation* 0
        *elapsed-ms* 0
        *delay* *default-delay*
        *running* t
        *last-tick* (sdl2:get-ticks)))

;;; MAIN
(defun run ()
  (let ((grid (make-grid *grid-width* *grid-height*)))
    (apply-seed grid 1)

    (sdl2:with-init (:video)
      (sdl2:with-window (window :title "Game of Life"
                                :w (* *grid-width* *cell-size*)
                                :h (* *grid-height* *cell-size*))
        (sdl2:with-renderer (renderer window)
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)

            ;; KEYBOARD
            (:keydown
             (:keysym keysym)
             (case (sdl2:scancode keysym)
               (:scancode-space (setf *running* (not *running*)))
               (:scancode-up (setf *delay* (max 10 (- *delay* 20))))
               (:scancode-down (incf *delay* 20))
               (:scancode-1 (apply-seed grid 1))
               (:scancode-2 (apply-seed grid 2))
               (:scancode-3 (apply-seed grid 3))
               (:scancode-4 (apply-seed grid 4))
               (:scancode-5 (apply-seed grid 5))))

            ;; MOUSE DRAW (LEFT BUTTON = 1)
            (:mousebuttondown
             (:button b :x mx :y my)
             (when (= b 1)
               (setf *mouse-down* t)
               (draw-cell grid mx my)))

            (:mousebuttonup
             (:button b)
             (when (= b 1)
               (setf *mouse-down* nil)))

            (:mousemotion
             (:x mx :y my)
             (when *mouse-down*
               (draw-cell grid mx my)))

            ;; LOOP
            (:idle ()
              (let ((now (sdl2:get-ticks)))
                (update-title window)

                (sdl2:set-render-draw-color renderer 0 0 0 255)
                (sdl2:render-clear renderer)

                (sdl2:set-render-draw-color renderer 255 255 255 255)
                (draw-grid renderer grid)

                (sdl2:render-present renderer)

                (when *running*
                  (incf *elapsed-ms* (- now *last-tick*))
                  (setf grid (next-generation grid))
                  (incf *generation*)
                  (sdl2:delay *delay*))

                (setf *last-tick* now)))))))))

