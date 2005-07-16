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
	    *terminal-velocity*))

  (incf (iso-point-x (actor-position a)) (iso-point-x (actor-velocity a)))
  (incf (iso-point-z (actor-position a)) (iso-point-z (actor-velocity a)))
  (incf (iso-point-y (actor-position a)) (iso-point-y (actor-velocity a)))

  (detect-collisions a)
  ;;; call contact handlers?

  (if (actor-contact-surface a)
       ;; standing on something.
       (progn
	 ;; only if in contact with something below us.
	 (sinkf (iso-point-x (actor-velocity a)) *ground-friction*)
	 (sinkf (iso-point-z (actor-velocity a)) *ground-friction*))

       ;; in the air.
       (progn
	 (sinkf (iso-point-x (actor-velocity a)) *air-friction*)
	 (sinkf (iso-point-z (actor-velocity a)) *air-friction*)
	 (incf (iso-point-y (actor-velocity a)) *gravity*))))


(defun detect-collisions (alice)
  (when (and (actor-contact-surface alice)
	     (not (penetrating-p alice (actor-contact-surface alice))))
    (setf (actor-contact-surface alice) nil))

  (maphash (lambda (unused bob)
	     (declare (ignore unused))
	     (when (and (not (eql alice bob)) (penetrating-p alice bob))
	       (multiple-value-bind (impulse lsa not-lsa)
		   (resolve-collision alice bob)
		 (incf (iso-point-component lsa (actor-velocity alice))
		       (iso-point-component lsa impulse))
		 (dolist (axis not-lsa)
		   (decf (iso-point-component axis (actor-position alice))
			 (iso-point-component axis impulse)))
		   
		 ;; when lsa is :y...
		 (when (and (eql lsa :y)
			    (> (iso-point-y (actor-position alice))
			       (iso-point-y (actor-position bob))))
		   (setf (actor-contact-surface alice) bob))

		 (awhen (find (actor-type bob) *actor-archetypes* :key #'car)
			(funcall (cadr (assoc :contact (cdr it)))
				 bob alice lsa impulse)))))
	   *actor-map*)

  (room-collision-detection alice))


(defun room-collision-detection (alice)
  (maphash
   (lambda (key bob)
     (declare (ignore key))
     (when (penetrating-p alice bob)
       (multiple-value-bind (impulse lsa not-lsa)
	   (resolve-collision alice bob)
	 (when lsa
	   (incf (iso-point-component lsa (actor-velocity alice))
		 (iso-point-component lsa impulse)))
	 (dolist (axis not-lsa)
	   (decf (iso-point-component axis (actor-position alice))
		 (iso-point-component axis impulse)))
      
	 ;; when lsa is :y...
	 (when (and (eql lsa :y)
		    (> (iso-point-y (actor-position alice))
		       (iso-point-y (actor-position bob))))
	   (setf (actor-contact-surface alice) bob)))))
   *room-block-actors*)

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
      (when (penetrating-p alice wall-obj)
	(multiple-value-bind (impulse lsa not-lsa)
	    (resolve-collision alice wall-obj)
	  (check-exit x z)
	  (assert lsa
		  (alice wall-obj)
		  "No separating axis between ~A and ~A!"
		  alice wall-obj)
	  (incf (iso-point-component lsa (actor-velocity alice))
		(iso-point-component lsa impulse))
	  (dolist (axis not-lsa)
	    (decf (iso-point-component axis (actor-position alice))
		  (iso-point-component axis impulse)))))))

  (when (<= (iso-point-y (actor-position alice)) *room-lowest-point*)
    (let ((floor-obj (make-floor-object x z)))
      (when (penetrating-p alice floor-obj)
	(multiple-value-bind (impulse lsa not-lsa)
	    (resolve-collision alice floor-obj)
	  (assert lsa
		  (alice floor-obj)
		  "No separating axis between ~A and ~A!"
		  alice floor-obj)
	  (incf (iso-point-component lsa (actor-velocity alice))
		(iso-point-component lsa impulse))
	  (dolist (axis not-lsa)
	    (decf (iso-point-component axis (actor-position alice))
		  (iso-point-component axis impulse))))
	(setf (actor-contact-surface alice) floor-obj))))

  (when (>= (+ (iso-point-y (actor-position alice))
	       (iso-point-y (box-dimensions (actor-box alice))))
	    *room-highest-point*)
    (let ((ceiling-obj (make-ceiling-object x z)))
      (when (penetrating-p alice ceiling-obj)
	(multiple-value-bind (impulse lsa not-lsa)
	    (resolve-collision alice ceiling-obj)
	  (assert lsa
		  (alice ceiling-obj)
		  "No separating axis between ~A and ~A!"
		  alice ceiling-obj)
	  (incf (iso-point-component lsa (actor-velocity alice))
		(iso-point-component lsa impulse))
	  (dolist (axis not-lsa)
	    (decf (iso-point-component axis (actor-position alice))
		  (iso-point-component axis impulse))))))))


(defun sign-of (v)
  (cond ((plusp v) 1)
	((minusp v) -1)
	(t nil)))

;; eventually this will use a cool trick like in game physics section
;; 5.3 or so.
(defun resolve-collision (alice bob)
  (let ((impulse #I(0 0 0)))
    ;; handle fractional component first.
    (dolist (axis '(:x :y :z))
      (let* ((f (iso-point-component-function-of axis))
	     (amin (iso-point-component axis (actor-position alice)))
	     (bmin (iso-point-component axis (actor-position bob))))
	(when
	    (extents-overlap-p
	     amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	     bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	  (multiple-value-bind (int frac)
	      (floor (funcall f (actor-velocity alice)))
	    (declare (ignore int))
	    (decf (iso-point-component axis (actor-position alice))
		  frac)
	    (decf (iso-point-component axis impulse) frac)))))

    ;; pixel by pixel step, otherwise.
    (do ((max-iterations 25 (1- max-iterations)))
	((or (not (penetrating-p alice bob))
	     (zerop max-iterations)))
      (dolist (axis '(:x :y :z))
	(let* ((f (iso-point-component-function-of axis))
	       (amin (funcall f (actor-position alice)))
	       (bmin (funcall f (actor-position bob))))
	  (when
	      (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	       bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	    (awhen (sign-of (funcall f (actor-velocity alice)))
		   (decf (iso-point-component axis
					      (actor-position alice)) it)
		   (decf (iso-point-component axis impulse) it))))))
    
    (let (lsa not-lsa)
      (dolist (axis '(:x :z :y))	; note order: prefer y to z or x.
	(let* ((f (iso-point-component-function-of axis))
	       (amin (funcall f (actor-position alice)))
	       (bmin (funcall f (actor-position bob))))
	  (unless (extents-overlap-p
	       amin (+ amin (funcall f (box-dimensions (actor-box alice))))
	       bmin (+ bmin (funcall f (box-dimensions (actor-box bob)))))
	    (setf lsa axis))))
      ;; sort of a temporary hack to try to fix problem where two LSAs
      ;; are set.
      (dolist (axis '(:x :y :z)) (unless (eql axis lsa) (push axis not-lsa)))
      (values impulse lsa not-lsa))))


(defun apply-impulse (actor &key (x 0.0) (y 0.0) (z 0.0))
  (incf (iso-point-x (actor-velocity actor)) x)
  (incf (iso-point-y (actor-velocity actor)) y)
  (incf (iso-point-z (actor-velocity actor)) z))


(defun report-axes-of-separation (alice-box bob-box)
  (let (separation)
    (dolist (axis '(:x :y :z))
      (let ((amin (iso-point-component axis (box-position alice-box)))
	    (bmin (iso-point-component axis (box-position bob-box))))
	(unless
	    (extents-overlap-p
	     amin (+ amin (iso-point-component axis
					       (box-dimensions alice-box)))
	     bmin (+ bmin (iso-point-component axis
					       (box-dimensions bob-box))))
	  (push axis separation))))
    separation))

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

