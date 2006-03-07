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

(defun update-physics (a room)
  "Update simple kinematics, friction, collision response on the given
actor."
  (dolist (axis '(:x :y :z))
    (clampf (iso-point-component axis (velocity-of a))
	    *terminal-velocity*)
    (incf (iso-point-component axis (position-of a))
	  (iso-point-component axis (velocity-of a))))

  (detect-collisions a room)

  (let ((friction (if (contact-surface-of a) *ground-friction* *air-friction*)))
    (sinkf (iso-point-x (velocity-of a)) friction)
    (sinkf (iso-point-z (velocity-of a)) friction))
  (incf (iso-point-y (velocity-of a)) *gravity*))


(defun detect-collisions (alice room)
  (when (and (contact-surface-of alice)
	     (not (penetrating-p alice (contact-surface-of alice))))
    (setf (contact-surface-of alice) nil))

  (maphash (lambda (unused bob) (declare (ignore unused)) (actor<->actor-collision alice bob))
	   *actor-map*)

  (maphash (lambda (unused bob) (declare (ignore unused)) (actor<->actor-collision alice bob))
	   *room-block-actors*)

  (room-collision-detection alice room))


(defun actor<->actor-collision (alice bob)
  (declare (optimize speed)
	   (type actor alice bob))
  "Returns T if the actors collided, NIL otherwise."
  (when (and (not (eql alice bob)) (penetrating-p alice bob))
    (multiple-value-bind (impulse lsa) (resolve-collision alice bob)

      (assert lsa (alice bob)
		  "No separating axis between ~A and ~A!"
		  alice bob)

      (collision-response alice lsa impulse)

      ;; make bob the floor.
      (when (and (eql lsa :y)
		 (> (iso-point-y (position-of alice))
		    (iso-point-y (position-of bob))))
	(setf (contact-surface-of alice) bob))

      (awhen (find (actor-type bob) *actor-archetypes* :key #'car)
	(funcall (cadr (assoc :contact (cdr it)))
		 bob alice lsa impulse)))
    t))

(defun collision-response (alice lsa impulse)
  "Affects collision response on ALICE."
  (incf (iso-point-component lsa (velocity-of alice))
	(iso-point-component lsa impulse))
  (dolist (axis (set-difference '(:x :y :z) (list lsa)))
    (decf (iso-point-component axis (position-of alice))
	  (iso-point-component axis impulse))))

(defun room-collision-detection (alice room)
  (let ((w (ceiling (iso-point-x (box-dimensions (box-of alice)))
		    +tile-size+))
	(d (ceiling (iso-point-z (box-dimensions (box-of alice)))
		    +tile-size+)))
    (do* ((base-z (floor (iso-point-z (position-of alice)) +tile-size+))
	  (z base-z (1+ z)))
	 ((> z (+ base-z d)))
      (do* ((base-x (floor (iso-point-x (position-of alice)) +tile-size+))
	    (x base-x (1+ x)))
	   ((> x (+ base-x w)))
	(try-border-collision room alice x z)))))

;; XXX fixme do floor and ceiling collisions, too.
(defun try-border-collision (room alice x z)
  (when (or (minusp x) (minusp z)
	    (>= x (width-of room))
	    (>= z (depth-of room))
	    (= (aref (floor-of room) z x) 0))
    (let ((wall-obj (make-wall-object x z)))
      (when (actor<->actor-collision alice wall-obj)
        (border-collision room alice x z))))

  (when (<= (iso-point-y (position-of alice)) *room-lowest-point*)
    (let ((floor-obj (make-floor-object x z)))
      (actor<->actor-collision alice floor-obj)))

  (when (>= (+ (iso-point-y (position-of alice))
	       (iso-point-y (box-dimensions (box-of alice))))
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
	     (amin (iso-point-component axis (position-of alice)))
	     (bmin (iso-point-component axis (position-of bob))))
	(when (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (box-of alice))))
	       bmin (+ bmin (funcall f (box-dimensions (box-of bob)))))
	  ,@body)))))

;; eventually this will use a cool trick like in game physics section
;; 5.3 or so.
(defun resolve-collision (alice bob)
  (let ((impulse #I(0 0 0)))
    ;; handle fractional component first.
    (try-penetration-resolution
     (multiple-value-bind (int frac) (floor (funcall f (velocity-of alice)))
       (declare (ignore int))
       (decf (iso-point-component axis (position-of alice)) frac)
       (decf (iso-point-component axis impulse) frac)))

    ;; pixel by pixel step, otherwise.
    (do ((max-iterations 25 (1- max-iterations))) ; XXX 25 is arbitrary.
	((or (not (penetrating-p alice bob))
	     (zerop max-iterations)))
      (try-penetration-resolution
       (awhen (sign-of (funcall f (velocity-of alice)))
	 (decf (iso-point-component axis (position-of alice)) it)
	 (decf (iso-point-component axis impulse) it))))
    
    (values impulse (find-axis-of-separation alice bob))))


(defun apply-impulse (actor &key (x 0.0) (y 0.0) (z 0.0))
  (incf (iso-point-x (velocity-of actor)) x)
  (incf (iso-point-y (velocity-of actor)) y)
  (incf (iso-point-z (velocity-of actor)) z))


(defun find-axis-of-separation (alice bob)
  (dolist (axis '(:y :z :x))	; note order: prefer y to z or x.
    (let* ((f (iso-point-component-function-of axis))
	   (amin (funcall f (position-of alice)))
	   (bmin (funcall f (position-of bob))))
      (unless (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (box-of alice))))
	       bmin (+ bmin (funcall f (box-dimensions (box-of bob)))))
	(return axis)))))

(defun penetrating-p (alice bob)
  (declare (type actor alice bob))
  (actor-pen-p (position-of alice)
	       (box-dimensions (box-of alice))
	       (position-of bob)
	       (box-dimensions (box-of bob))))

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

