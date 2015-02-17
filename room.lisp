;;;
;;; room.lisp -- Room management for Equinox-ish games.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :aequus-noctis)

(defparameter *room-lowest-point* 0)
(defparameter *room-highest-point* 256)
(defparameter *floor-slice-y* -16)
(defparameter *slice-height-increment* 32)
(defparameter *floor-tile-height* 16)

(defvar *floor-buffer* nil)
;;; Note that the actors list gets eval'd -- this allows random
;;; placement and other fun.
(defvar *room-set* nil)
(defvar *current-room*)


(defun initialize-room-data (&optional (rooms-file "rooms.sexp"))
  (with-open-file (stream rooms-file)
    (setf *room-set* (read stream))))

(defparameter *max-blocks-per-room* 256)
(defparameter *max-actors-per-room* 64)

;;; XXX the idea of player-spawn, exits, and name are going to be moved to
;;; the scenario logic.
(defclass room ()
  ((floor :accessor floor-of)
   (blocks :accessor blocks-of
           :initform (make-array *max-blocks-per-room* :adjustable nil :fill-pointer 0 :element-type 'actor))
   (actors :accessor actors-of
           :initform (make-array *max-actors-per-room* :adjustable nil :fill-pointer 0 :element-type 'actor))
   (sprites
    :initform (make-array (+ *max-blocks-per-room* *max-actors-per-room*) :adjustable nil :fill-pointer 0 :element-type 'fetus:sprite))
   (backdrop :accessor backdrop-of :initarg :backdrop
             :documentation "The flat RGBA color underneath everything else.")
   (archetype :accessor archetype-of)
   (name :accessor room-name))
  (:documentation "ROOM encapsulates the concept of a location; a
floor, fixed blocks (set), and actors.")
  (:default-initargs :backdrop #xff101010))

;; define make-room from plan

(defun load-room-int (room name)
  "Loads the named room from *ROOM-SET*, into *CURRENT-ROOM*.
Prerenders floor, adds fixed blocks to SPRITE-MANAGER, and optionally
 (based on SPAWN-ACTORS-P) spawns actors associated with room."
  (setf *wall-objects* (make-hash-table)
	*floor-objects* (make-hash-table)
	*ceiling-objects* (make-hash-table))
  (let ((archetype (assoc name *room-set*)))
    (assert archetype () "Couldn't find room ~A." name)
    (setf (floor-of room) (cdr (assoc :floor (cdr archetype)))
          (room-name room) (cdr (assoc :name (cdr archetype)))
          (archetype-of room) archetype)

    ;; XXX deal with physics constants here.
    (fetus:use-image-palette (image-of (aref *tiles* 1)))

    (paint-floor room)

    (dolist (block (cdr (assoc :blocks (cdr archetype))))
      (give-block-sprite room block))
    room))

(defun add-actor-to-room (room actor)
  (vector-push actor (actors-of room)))

;;; Be warned here and in REMOVE-BLOCK-FROM-ROOM; it is not specified
;;; that DELETE returns the same array with its fill-pointer intact,
;;; so we might have to do something more clever here.  As it is, I am
;;; going to wait until I actually have that happen with an
;;; implementation we use before I do that more-clever-thing, though.
;;; (And I might instead submit a patch to the implementation that
;;; preserves the fill-pointer)
(defun remove-actor-from-room (room actor)
  (delete actor (actors-of room)))

(defun add-block-to-room (room block)
  (vector-push block (blocks-of room)))

(defun remove-block-from-room (room block)
  (delete block (blocks-of room)))

(defmethod width-of ((room room))
  (array-dimension (floor-of room) 1))

(defmethod depth-of ((room room))
  (array-dimension (floor-of room) 0))


(defmethod paint ((room room) (camera camera))
  (fetus:fill-background (fetus:get-local-color (backdrop-of room)))
  (with-slots (x y height) camera
    (fetus:blit-image *floor-buffer*
                      (- x (half (fetus:surface-w *floor-buffer*)))
                      y))
  (with-slots (sprites) room
    (setf (fill-pointer sprites) 0)
    (loop for block across (blocks-of room)
          do (update-sprite-coords sprites block camera))
    (loop for actor across (actors-of room)
          do (update-sprite-coords sprites actor camera))
    (setf sprites (stable-sort sprites
                               #'isometric-sprite-cmp
                               :key #'fetus:sprite-priority))
    (loop for sprite across sprites
          do (fetus:draw-sprite sprite))))


(defmethod update ((room room) where time-elapsed)
  "Update collisions, physics, and handlers for all actors registered
with the actor manager."
  ;; WHERE in the context of rooms is the PLAY-SESSION, but we don't
  ;; care about that here.
  (declare (ignore where))
  (loop for actor across (actors-of room)
        do (progn
             (update-physics actor room time-elapsed)
             (ensure-no-penetrations actor room)
             ;; XXX update contact handlers
             (update actor room time-elapsed))))

(defun ensure-no-penetrations (alice room)
  (loop for bob across (actors-of room)
        unless (eq alice bob)
        do (assert (not (penetrating-p alice bob)))))


;;;; FLOORS

(defun paint-floor (room)
  "function PAINT-FLOOR

Paint floor tiles according to room.  Tiles at +TILE-SIZE+ intervals;
paints from back to front."

  ;; Draw order is from bottom-right of the array (furthest away from
  ;; the camera).
  (let* ((h-extent (width-of room))
	 (v-extent (depth-of room))
	 (h-offs 0) (v-offs 0) (h-max 0) (v-max 0))

    (setf h-max (* h-extent 64)) ;; XXX constants
    (setf v-max (* v-extent 32))
    (incf h-max 64)
    (incf v-max 64)
    (setf h-offs (half h-max))
    (setf v-offs (+ (half v-max) 128))

    ;; blit offset
    (decf v-offs 8)

    (when *floor-buffer*
      (fetus:free-image *floor-buffer*))
    (setf *floor-buffer* (fetus:new-image-buffer h-max v-max))
    (fetus:fill-background 0 *floor-buffer*)

    (paint-floor-internal (floor-of room)
                          *floor-buffer*
			  h-extent h-offs
			  v-extent v-offs)))

(defun paint-floor-internal (floor buffer h-extent h-offs v-extent v-offs)
  (loop for z from (1- v-extent) downto 0
	with pt = (make-iso-point)
	do (loop for x from (1- h-extent) downto 0
		 for tile = (let ((idx (aref floor z x)))
			      (when (plusp idx) (aref *tiles* idx)))
		 when tile
                   do (setf (iso-point-x pt) (* +tile-size+ x)
                            (iso-point-y pt) 0
                            (iso-point-z pt) (* +tile-size+ z))
                      (multiple-value-bind (u v)
                          (iso-project-point pt)
                        (incf v +tile-size+)
                        (let* ((sprite (sprite-of (archetype-of tile)))
                               (blit-offset (cadr (assoc :blit-offset sprite))))
                          (decf u (car blit-offset))
                          (decf v (cdr blit-offset))
                          (incf u h-offs)
                          (incf v v-offs)
                          (fetus:blit-image (image-of tile) u v
                                            :destination buffer))))))

(defmethod sprite-of ((archetype list))
  (cdr (assoc :sprite (cdr archetype))))

(defun position-hash-key (x z)
  (complex x z))

(defclass impassable-body (actor) ())
;;; While I'd prefer to name these WALL, FLOOR, and CEILING, the
;;; inevitable conflicts with CL:FLOOR and so on are ones I'd like to
;;; avoid dealing with.
(defclass wall-object (impassable-body) ())
(defclass floor-object (impassable-body) ())
(defclass ceiling-object (impassable-body) ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *wall-objects* (make-hash-table)))
(defun make-wall-object (x z)
  (let ((objects *wall-objects*))
    (unless (gethash (position-hash-key x z) objects)
      (let ((wall (make-instance 'wall-object
				 :position #I((* x +tile-size+) 0
					      (* z +tile-size+))
				 :box (make-box :position #I(0 0 0)
						:dimensions #I(+tile-size+
							       *room-highest-point*
							       +tile-size+)))))
	(setf (gethash (position-hash-key x z) objects) wall)))
    (gethash (position-hash-key x z) objects)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *floor-objects* (make-hash-table)))
(defun make-floor-object (x z)
  (let ((objects *floor-objects*))
    (unless (gethash (position-hash-key x z) objects)
      (let ((floor (make-instance 'floor-object)))
	(setf (position-of floor) #I((* x +tile-size+) -16
				     (* z +tile-size+))
	      (box-of floor) (make-box :position #I(0 0 0)
				       :dimensions #I(+tile-size+
						      16
						      +tile-size+))
	      (gethash (position-hash-key x z) *floor-objects*) floor)))
    (gethash (position-hash-key x z) objects)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *ceiling-objects* (make-hash-table)))
(defun make-ceiling-object (x z)
  (let ((objects *ceiling-objects*))
    (unless (gethash (position-hash-key x z) objects)
      (let ((ceiling (make-instance 'ceiling-object)))
	(setf (position-of ceiling) #I((* x +tile-size+)
					  *room-highest-point*
					  (* z +tile-size+))
	      (box-of ceiling)
	      (make-box :position #I(0 0 0)
			:dimensions #I(+tile-size+
				       64
				       +tile-size+))
	      (gethash (position-hash-key x z) objects) ceiling)))
    (gethash (position-hash-key x z) objects)))


;;;; BLOCKS

(defun give-block-sprite (room block)
  (let* ((arch (cdr (archetype-of (aref *tiles* (first block)))))
	 (actor (make-slice-object arch (second block)
				   (third block) (fourth block)))
	 (sprite (fetus:new-sprite-from-alist (cdr (assoc :sprite arch)))))
    (setf (sprite-of actor) sprite)
    (add-block-to-room room actor)))

(defun make-slice-object (archetype x y z)
  (let ((block (make-instance 'impassable-body)))
    (setf (position-of block) #I((* x +tile-size+)
				 (* y *slice-height-increment*)
				 (* z +tile-size+)))
    (destructuring-bind ((x y z) (w h d)) (cdr (assoc :box archetype))
      (setf (box-of block)
	    (make-box :position (make-iso-point :x x :y y :z z)
		      :dimensions (make-iso-point :x w :y h :z d))))
    block))
