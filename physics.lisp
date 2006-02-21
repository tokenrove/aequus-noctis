;;;
;;; physics.lisp -- Physics routines.
;;;
;;; A crude hack, but it should work for Equinox-ish games.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :aequus-noctis)

;;; XXX Each of these constants is totally arbitrary.
(defparameter *air-friction* 0.25
  "Resistance against actors while in the air.")
(defparameter *ground-friction* 0.25
  "Friction on the floor.  Note that this parameter will not be here
forever as friction becomes delegated to surfaces.")
(defparameter *gravity* -0.5
  "Gravity affecting actors.")
(defparameter *terminal-velocity* 16
  "Terminal velocity (currently, in any direction).")

(defun update-physics (a)
  "Update simple kinematics, friction, collision response on the given
actor."
  (dolist (axis '(:x :y :z))
    (clampf (iso-point-component axis (actor-velocity a))
	    *terminal-velocity*)
    (incf (iso-point-component axis (actor-position a))
	  (iso-point-component axis (actor-velocity a))))

  (detect-collisions a)

  (let ((friction (if (actor-contact-surface a) *ground-friction* *air-friction*)))
    (sinkf (iso-point-x (actor-velocity a)) friction)
    (sinkf (iso-point-z (actor-velocity a)) friction))
  (incf (iso-point-y (actor-velocity a)) *gravity*))


(defun detect-collisions (alice)
  (when (and (actor-contact-surface alice)
	     (not (penetrating-p alice (actor-contact-surface alice))))
    (setf (actor-contact-surface alice) nil))

  (maphash (lambda (unused bob) (declare (ignore unused)) (actor<->actor-collision alice bob))
	   *actor-map*)

  (maphash (lambda (unused bob) (declare (ignore unused)) (actor<->actor-collision alice bob))
	   *room-block-actors*)

  (room-collision-detection alice))


(defun actor<->actor-collision (alice bob)
  "Returns T if the actors collided, NIL otherwise."
  (when (and (not (eql alice bob)) (penetrating-p alice bob))
    (multiple-value-bind (impulse lsa) (resolve-collision alice bob)

      (assert lsa (alice bob)
		  "No separating axis between ~A and ~A!"
		  alice bob)

      (collision-response alice lsa impulse)
		   
      ;; make bob the floor.
      (when (and (eql lsa :y)
		 (> (iso-point-y (actor-position alice))
		    (iso-point-y (actor-position bob))))
	(setf (actor-contact-surface alice) bob))

      (awhen (find (actor-type bob) *actor-archetypes* :key #'car)
	(funcall (cadr (assoc :contact (cdr it)))
		 bob alice lsa impulse)))
    t))
	
(defun collision-response (alice lsa impulse)
  "Affects collision response on ALICE."
  (incf (iso-point-component lsa (actor-velocity alice))
	(iso-point-component lsa impulse))
  (dolist (axis (set-difference '(:x :y :z) (list lsa)))
    (decf (iso-point-component axis (actor-position alice))
	  (iso-point-component axis impulse))))

(defun room-collision-detection (alice)
  (let ((w (ceiling (iso-point-x (box-dimensions (actor-box alice)))
		    +tile-size+))
	(d (ceiling (iso-point-z (box-dimensions (actor-box alice)))
		    +tile-size+)))
    (do* ((base-z (floor (iso-point-z (actor-position alice)) +tile-size+))
	  (z base-z (1+ z)))
	 ((> z (+ base-z d)))
      (do* ((base-x (floor (iso-point-x (actor-position alice)) +tile-size+))
	    (x base-x (1+ x)))
	   ((> x (+ base-x w)))
	(try-border-collision alice x z)))))

;;; XXX this is gross.	
(defun check-exit (x z)
  (when (minusp x) (setf x 0))
  (when (minusp z) (setf z 0))
  (when (>= x (1- (room-width))) (setf x (1- (room-width))))
  (when (>= z (1- (room-depth))) (setf z (1- (room-depth))))

  (dolist (exit (room-exits *current-room*))
    (let ((this-side (first exit)))
      (when (and (consp this-side)
		 (= x (car this-side))
		 (= z (cdr this-side)))
	(when (zerop *exit-counter*)
	  (setf *magic-exit-hack* (list (second exit) (third exit))))))))


(defun try-border-collision (alice x z)
  (when (or (minusp x) (minusp z)
	    (>= x (room-width))
	    (>= z (room-depth))
	    (= (aref (room-floor *current-room*) z x) 0))
    (let ((wall-obj (make-wall-object x z)))
      (when (actor<->actor-collision alice wall-obj)
	  (check-exit x z))))

  (when (<= (iso-point-y (actor-position alice)) *room-lowest-point*)
    (let ((floor-obj (make-floor-object x z)))
      (actor<->actor-collision alice floor-obj)))

  (when (>= (+ (iso-point-y (actor-position alice))
	       (iso-point-y (box-dimensions (actor-box alice))))
	    *room-highest-point*)
    (let ((ceiling-obj (make-ceiling-object x z)))
      (actor<->actor-collision alice ceiling-obj))))


(defun sign-of (v)
  "Returns 1 when V is positive, -1 when negative, and NIL otherwise."
  (cond ((plusp v) 1)
	((minusp v) -1)
	(t nil)))

(eval-when (:compile-toplevel)
  (defmacro try-penetration-resolution (&body body)
    `(dolist (axis '(:x :y :z))
      (let* ((f (iso-point-component-function-of axis))
	     (amin (iso-point-component axis (actor-position alice)))
	     (bmin (iso-point-component axis (actor-position bob))))
	(when (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	       bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	  ,@body)))))

;; eventually this will use a cool trick like in game physics section
;; 5.3 or so.
(defun resolve-collision (alice bob)
  (let ((impulse #I(0 0 0)))
    ;; handle fractional component first.
    (try-penetration-resolution
     (multiple-value-bind (int frac) (floor (funcall f (actor-velocity alice)))
       (declare (ignore int))
       (decf (iso-point-component axis (actor-position alice)) frac)
       (decf (iso-point-component axis impulse) frac)))

    ;; pixel by pixel step, otherwise.
    (do ((max-iterations 25 (1- max-iterations))) ; XXX 25 is arbitrary.
	((or (not (penetrating-p alice bob))
	     (zerop max-iterations)))
      (try-penetration-resolution
       (awhen (sign-of (funcall f (actor-velocity alice)))
	 (decf (iso-point-component axis (actor-position alice)) it)
	 (decf (iso-point-component axis impulse) it))))
    
    (values impulse (find-axis-of-separation alice bob))))


(defun apply-impulse (actor &key (x 0.0) (y 0.0) (z 0.0))
  (incf (iso-point-x (actor-velocity actor)) x)
  (incf (iso-point-y (actor-velocity actor)) y)
  (incf (iso-point-z (actor-velocity actor)) z))


(defun find-axis-of-separation (alice bob)
  (dolist (axis '(:y :z :x))	; note order: prefer y to z or x.
    (let* ((f (iso-point-component-function-of axis))
	   (amin (funcall f (actor-position alice)))
	   (bmin (funcall f (actor-position bob))))
      (unless (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	       bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	(return axis)))))

(defun penetrating-p (alice bob)
  (declare (type actor alice bob))
  (actor-pen-p (actor-position alice)
	       (box-dimensions (actor-box alice))
	       (actor-position bob)
	       (box-dimensions (actor-box bob))))

(defun actor-pen-p (a-pos a-dim b-pos b-dim)
  (declare (type iso-point a-pos a-dim b-pos b-dim))
  (and (let ((xa (iso-point-x a-pos))
	     (xb (iso-point-x b-pos)))
	 (extents-overlap-p xa (+ xa (iso-point-x a-dim))
			    xb (+ xb (iso-point-x b-dim))))
       (let ((ya (iso-point-y a-pos))
	     (yb (iso-point-y b-pos)))
	 (extents-overlap-p ya (+ ya (iso-point-y a-dim))
			    yb (+ yb (iso-point-y b-dim))))
       (let ((za (iso-point-z a-pos))
	     (zb (iso-point-z b-pos)))
	 (extents-overlap-p za (+ za (iso-point-z a-dim))
			    zb (+ zb (iso-point-z b-dim))))))

