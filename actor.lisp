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

(defvar *actor-map* (make-hash-table)
  "Global hash of ID => ACTOR containing each actor ``alive'' in the
game world.")
(defvar *actor-id-counter* 0
  "Actor unique ID counter, should always contain an ID which is not
currently in use by any live actors.")

(defun initialize-actor-data (&optional (archetypes-file "archetypes.sexp"))
  (let ((*package* (find-package :equinox))
	(*read-eval* nil))
    (with-open-file (stream archetypes-file)
      (setf *actor-archetypes* (read stream)))))

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
  (let* ((archetype (or (cdr (find name *actor-archetypes* :key #'car))
			(error "archetype ~A not found" name)))
	 (box (destructuring-bind ((x y z) (w h d))
		  (cdr (assoc :box archetype))
		(make-box :position (make-iso-point :x x :y y :z z)
			  :dimensions (make-iso-point :x w :y h :z d))))
	 (actor (make-instance 'actor :type name
			       :position position
                               :sprite (fetus:new-sprite-from-alist
					(cdr (assoc :sprite archetype)))
			       :box box)))
    (manage-actor actor)
    (fetus:add-sprite-to-manager sprite-manager (sprite-of actor))
    actor))

(defun initialize-actor-from-archetype (actor position sprite-manager archetype)
  (let* ((box (destructuring-bind ((x y z) (w h d))
		  (cdr (assoc :box archetype))
		(make-box :position (make-iso-point :x x :y y :z z)
			  :dimensions (make-iso-point :x w :y h :z d)))))
    (setf (position-of actor) position
          (sprite-of actor) (fetus:new-sprite-from-alist
			     (cdr (assoc :sprite archetype)))
	  (box-of actor) box)
    (manage-actor actor)
    (fetus:add-sprite-to-manager sprite-manager (sprite-of actor))
    actor))


(defun ensure-no-penetrations (id actor)
  (maphash (lambda (id-b actor-b)
	     (unless (= id id-b)
	       (assert (not (penetrating-p actor actor-b)))))
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
  (print-unreadable-object (actor stream :type t :identity t)
    (format stream "TYPE(~A) POSITION(~A)" (actor-type actor) (position-of actor))))
