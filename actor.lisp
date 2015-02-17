;;; actor.lisp -- Actor management code for Aequus Noctis (engine for
;;; Demon of the Fall).
;;;
;;; Defines the actor class, deals with global actor list, actor
;;; handlers, et cetera.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;


(in-package :aequus-noctis)

;;;; Actors

;; XXX add documentation for these slots.
(defclass actor ()
  ((type :reader actor-type :initarg :type :initform (error "Actors need types."))
   (sprite :accessor sprite-of :initarg :sprite)
   (position :accessor position-of :initarg :position)
   (velocity :accessor velocity-of :initform (make-iso-point))
   (box :accessor box-of :initarg :box)
   (contact-surface :accessor contact-surface-of :initform nil)
   (facing :accessor facing-of :initform :east))
  (:documentation "An ACTOR is an object that exists at the game-logic
level persistently.  Actors have handlers that are called at each time
slice in the game, handlers that are called in response to collisions,
and physical properties."))

;;; Fall-back handlers
(defmethod update ((who actor) where time-elapsed)
  (declare (ignore who where time-elapsed)))
(defmethod notify ((who actor) where what &key &allow-other-keys)
  (declare (ignore who where what)))

;;; XXX this function does not pay attention to box position.
(defun update-sprite-coords (sprites actor camera)
  "Update sprite screen coordinates from world coordinates."
  (with-slots (sprite position) actor
    (multiple-value-bind (u v) (iso-project-point position)
      (with-slots (x y) camera
        (incf u x)
        (incf v y))
     (setf (fetus:sprite-x sprite) (- u (car (fetus:sprite-blit-offset sprite)))
           (fetus:sprite-y sprite) (- v (cdr (fetus:sprite-blit-offset sprite))))
     (setf (fetus:sprite-priority sprite) actor)
     (vector-push sprite sprites))))


(defun isometric-sprite-cmp (a b)
  (let ((adim (box-dimensions (box-of a)))
	(bdim (box-dimensions (box-of b))))
    ;; if z overlap, then do more intensive tests.
    ;; otherwise, sort by z.
    (if (extents-overlap-p #1=(iso-point-z (position-of a))
			   (+ #1# (iso-point-z adim))
			   #2=(iso-point-z (position-of b))
			   (+ #2# (iso-point-z bdim)))
	(if (extents-overlap-p #3=(iso-point-x (position-of a))
			       (+ #3# (iso-point-x adim))
			       #4=(iso-point-x (position-of b))
			       (+ #4# (iso-point-x bdim)))
	    (<= (iso-point-y (position-of a))
		(iso-point-y (position-of b)))
	    (>= #3# #4#))
	(>= #1# #2#))))


(defmethod print-object ((actor actor) stream)
  (flet ((maybe-unbound (o s) (if (slot-boundp o s) (slot-value o s) :unbound)))
    (print-unreadable-object (actor stream :type t :identity t)
      (format stream "TYPE(~A) POSITION(~A)"
              (maybe-unbound actor 'type)
              (maybe-unbound actor 'position)))))
