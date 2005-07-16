;;; The contents of this file is due to be drastically changed.  These
;;; so-called utilities are really, at best, debugging utilities, and
;;; at worst, secretly editor/UI component helpers.

(in-package :aequus-noctis)

(defvar *camera* (cons 100 140))
(defvar *camera-follow*)
(defvar *magic-exit-hack* nil)
(defvar *exit-counter* 0)


(defun debugging-line-draw (point-1 point-2 origin)
  (multiple-value-bind (x1 y1)
      (iso-project-point (iso-point-translate point-1 origin))
    (incf x1 (car *camera*))
    (incf y1 (cdr *camera*))
    (when (minusp x1) (setf x1 0))
    (when (minusp y1) (setf y1 0))
    (multiple-value-bind (x2 y2)
	(iso-project-point (iso-point-translate point-2 origin))
      (incf x2 (car *camera*))
      (incf y2 (cdr *camera*))
      (when (minusp x2) (setf x2 0))
      (when (minusp y2) (setf y2 0))
      (fetus:draw-line x1 y1 x2 y2 255))))

(defun draw-bottom-cursor (box)
  (let* ((x (iso-point-x (box-dimensions box)))
	 ;(y (- (iso-point-y (box-dimensions box))))
	 (z (iso-point-z (box-dimensions box)))
	 (pos (box-position box)))
    ;; back
    (debugging-line-draw #i(0 0 z) #i(0 0 0) pos)
    (debugging-line-draw #i(0 0 z) #i(x 0 z) pos)
    (debugging-line-draw #i(0 0 0) #i(x 0 0) pos)
    (debugging-line-draw #i(x 0 0) #i(x 0 z) pos)))

(defun draw-top-cursor (box)
  (let* ((x (iso-point-x (box-dimensions box)))
	 (y (iso-point-y (box-dimensions box)))
	 (z (iso-point-z (box-dimensions box)))
	 (pos (box-position box)))
    ;; back
    (debugging-line-draw #i(0 y z) #i(0 y 0) pos)
    (debugging-line-draw #i(0 y z) #i(x y z) pos)
    (debugging-line-draw #i(0 y 0) #i(x y 0) pos)
    (debugging-line-draw #i(x y 0) #i(x y z) pos)))

(defun draw-debug-box (box &key partial)
  (let* ((x (iso-point-x (box-dimensions box)))
	 (y (iso-point-y (box-dimensions box)))
	 (z (iso-point-z (box-dimensions box)))
	 (pos (box-position box)))
    ;; back
    (unless partial
      (debugging-line-draw #i(x y z) #i(x y 0) pos)
      (debugging-line-draw #i(x y 0) #i(x 0 0) pos)
      (debugging-line-draw #i(0 y 0) #i(x y 0) pos)
      (debugging-line-draw #i(0 0 0) #i(x 0 0) pos)
      (debugging-line-draw #i(x 0 z) #i(x 0 0) pos))
    ;; front
    (debugging-line-draw #i(0 0 z) #i(0 0 0) pos)
    (debugging-line-draw #i(0 0 0) #i(x 0 0) pos)
    (debugging-line-draw #i(0 y z) #i(0 y 0) pos)
    (debugging-line-draw #i(0 y 0) #i(x y 0) pos)

    (debugging-line-draw #i(0 0 0) #i(0 y 0) pos)
    (debugging-line-draw #i(0 0 z) #i(0 y z) pos)
    (debugging-line-draw #i(x 0 0) #i(x y 0) pos)

    (debugging-line-draw #i(0 y z) #i(x y z) pos)
    (debugging-line-draw #i(x y 0) #i(x y z) pos)))
