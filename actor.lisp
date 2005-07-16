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
  ((type :reader actor-type)
   (sprite :accessor actor-sprite)
   (position :accessor actor-position)
   (velocity :accessor actor-velocity)
   (box :accessor actor-box)
   (handler :accessor actor-handler)
   (contact-surface :accessor actor-contact-surface)
   (facing :accessor actor-facing))
  (:documentation "An ACTOR is an object that exists at the game-logic
level persistently.  Actors have handlers that are called at each time
slice in the game, handlers that are called in response to collisions,
and physical properties."))

(defvar *actor-map* (make-hash-table)
  "Global hash of ID => ACTOR containing each actor ``alive'' in the
game world.")
(defvar *actor-id-counter* 0
  "Actor unique ID counter, should always contain an ID which is not
currently in use by any live actors.")

(defun for-each-actor (manager function)
  (maphash function (actor-manager-map manager)))

(defun initialize-actor-data (&optional (archetypes-file "archetypes.sexp"))
  (with-open-file (stream archetypes-file)
    (setf *actor-archetypes* (read stream))))

(defun create-actor-manager ()
  "function CREATE-ACTOR-MANAGER

Initialize the global actor manager.  Note that this doesn't check
whether it has previously been initialized."
  (setf *actor-map* (make-hash-table))
  (setf *actor-id-counter* 0))

(defun manage-actor (actor)
  "function MANAGE-ACTOR actor => id

Register actor with actor manager."
  (setf (gethash *actor-id-counter* *actor-map*) actor)
  (prog1 *actor-id-counter*
    (incf *actor-id-counter*)))

(defun unmanage-actor (id)
  "function UNMANAGE-ACTOR id => boolean

Remove the actor specified by id from the actor manager."
  (remhash id *actor-map*))

(defvar *actor-archetypes* nil
  "The actor archetypes table, which defines the default values for
many parameters of an actor.")


(defun spawn-actor-from-archetype (name position sprite-manager)
  "function SPAWN-ACTOR-FROM-ARCHETYPE name position sprite-manager => actor

Creates (and returns) a new ACTOR instance, reading default member
values from *ACTOR-ARCHETYPES*."
  (let ((actor (make-instance 'actor))
	(archetype (cdr (find name *actor-archetypes* :key #'car))))
    (when (null archetype)
      (error "archetype ~A not found" name))
    (setf (slot-value actor 'type) name
	  (actor-position actor) position
	  (actor-handler actor) (funcall (cadr (assoc :handler archetype)))
	  (actor-contact-surface actor) nil
	  (actor-facing actor) :east
	  (actor-sprite actor) (fetus:new-sprite-from-alist
				(cdr (assoc :sprite archetype))))
    (fetus:add-sprite-to-manager sprite-manager (actor-sprite actor))
    (destructuring-bind ((x y z) (w h d)) (cdr (assoc :box archetype))
      (setf (actor-box actor)
	    (make-box :position (make-iso-point :x x :y y :z z)
		    :dimensions (make-iso-point :x w :y h :z d))))
    (setf (actor-velocity actor) (make-iso-point))
    (manage-actor actor)
    actor))


(defun update-all-actors (foo)
  "Update collisions, physics, and handlers for all actors registered
with the actor manager."
  (declare (ignore foo))
  (maphash (lambda (id actor)
	     (update-physics actor)

	     ;; ensure no penetrations.
	     (maphash (lambda (id-b actor-b)
			(unless (= id id-b)
			  (assert (not (penetrating-p actor actor-b)))))
		      *actor-map*)

	     ;; XXX update contact handlers
	     ;; XXX camera
	     (update-sprite-coords (actor-sprite actor)
				   (actor-position actor)
				   actor)
	     (funcall (actor-handler actor) id actor))
	   *actor-map*))


;;; XXX this function does not pay attention to box position.
(defun update-sprite-coords (sprite position actor)
  "Update sprite screen coordinates from world coordinates."
  (multiple-value-bind (u v) (iso-project-point position)
    (incf u (car *camera*))
    (incf v (cdr *camera*))
    (setf (fetus:sprite-x sprite) (- u (car (fetus:sprite-blit-offset sprite)))
	  (fetus:sprite-y sprite) (- v (cdr (fetus:sprite-blit-offset sprite))))
    (setf (fetus:sprite-priority sprite) actor)))


(defun isometric-sprite-cmp (a b)
  (let ((adim (box-dimensions (actor-box a)))
	(bdim (box-dimensions (actor-box b))))
    ;; if z overlap, then do more intensive tests.
    ;; otherwise, sort by z.
    (if (extents-overlap-p #1=(iso-point-z (actor-position a))
			   (+ #1# (iso-point-z adim))
			   #2=(iso-point-z (actor-position b))
			   (+ #2# (iso-point-z bdim)))
	(if (extents-overlap-p #3=(iso-point-x (actor-position a))
			       (+ #3# (iso-point-x adim))
			       #4=(iso-point-x (actor-position b))
			       (+ #4# (iso-point-x bdim)))
	    (<= (iso-point-y (actor-position a))
		(iso-point-y (actor-position b)))
	    (>= #3# #4#))
	(>= #1# #2#))))


;;;; Handlers

(defun create-do-nothing-handler ()
  "Create an actor handler which does nothing."
  (lambda (id actor) (declare (ignore id actor))))

(defun create-floating-block-handler ()
  "Create an actor handler which floats up and down."
  (let ((direction :up))
    (lambda (id actor)
      (declare (ignore id))
      (when (actor-contact-surface actor)
	(setf direction :up))
      (if (eql direction :up)
	  (if (< (iso-point-y (actor-position actor)) 42)
	      (setf (iso-point-y (actor-velocity actor)) 0.7)
	      (setf direction :down))
	  (if (> (iso-point-y (actor-position actor)) 0)
	      (setf (iso-point-y (actor-velocity actor)) -0.2)
	      (setf direction :up))))))

(defun create-human-input-handler ()
  "Create a handler which updates an actor based on current input
events."
  (lambda (id player)
    (declare (ignore id))
    (when (fetus:event-pressedp +ev-up+)
      (apply-impulse player :x 0.5)
      (fetus:set-sprite-animation (actor-sprite player) :walk-north))
    (when (fetus:event-pressedp +ev-down+)
      (apply-impulse player :x -0.5)
      (fetus:set-sprite-animation (actor-sprite player) :walk-south))
    (when (fetus:event-pressedp +ev-left+)
      (apply-impulse player :z 0.5)
      (fetus:set-sprite-animation (actor-sprite player) :walk-west))
    (when (fetus:event-pressedp +ev-right+)
      (apply-impulse player :z -0.5)
      (fetus:set-sprite-animation (actor-sprite player) :walk-east))
    (when (and (fetus:event-pressedp +ev-button-a+)
	       (actor-contact-surface player))
      (apply-impulse player :y 6))))

(defun create-monster-handler ()
  "Create an actor handler which roves around in a sinister manner."
  ;; XXX unimplemented.
  (lambda (id actor) (declare (ignore id actor))))


(defun pushable-block-handler (us them face impulse)
  (declare (ignore them))
  (decf (iso-point-component face (actor-velocity us))
	(iso-point-component face impulse)))

(defun player-contact-handler (us them face impulse)
  (declare (ignore them))
  (decf (iso-point-component face (actor-velocity us))
	(iso-point-component face impulse)))

(defun monster-contact-handler (us them face impulse)
  (declare (ignore us them face impulse))
;;    if something is on top of us,
;;        sink its horizontal velocities by our friction,
;;        add our velocity to its velocity.
;;    if we're touching the player at all,
;;        kill them.
  )

(defun loot-contact-handler (us them face impulse)
  (declare (ignore us them face impulse))
  ;; if we're touching the player, they can have us.
  )
