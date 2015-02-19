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

(defparameter *max-blocks-per-room* 256)
(defparameter *max-actors-per-room* 64)

(defclass blueprint ()
  ((floor :accessor floor-of :initarg :floor)
   (blocks :accessor blocks-of :initarg :blocks))
  (:documentation "The plan for a ROOM."))

(defun blueprint-from-alist (alist)
  (make-instance 'blueprint
                 :floor (cdr (assoc :floor (cdr alist)))
                 :blocks (cdr (assoc :blocks (cdr alist)))))

;;; XXX the idea of player-spawn, exits, and name are going to be moved to
;;; the scenario logic.
(defclass room ()
  (floor-buffer wall-objects floor-objects ceiling-objects
   (blocks :accessor blocks-of
           :initform (make-array *max-blocks-per-room*
                                 :adjustable nil :fill-pointer 0 :element-type 'actor))
   (actors :accessor actors-of
           :initform (make-array *max-actors-per-room*
                                 :adjustable nil :fill-pointer 0 :element-type 'actor))
   (sprites
    :initform (make-array (+ *max-blocks-per-room* *max-actors-per-room*)
                          :adjustable nil :fill-pointer 0 :element-type 'fetus:sprite))
   (backdrop :accessor backdrop-of :initarg :backdrop
             :documentation "The flat RGBA color underneath everything else.")
   (blueprint :accessor blueprint-of :initarg :blueprint)
   (gravity :accessor gravity-of :initarg :gravity))
  (:documentation "ROOM encapsulates the concept of a location; a
floor, fixed blocks (set), and actors.")
  (:default-initargs :backdrop #xff101010 :gravity -0.25))

;; define make-room from plan

(defun make-room (blueprint)
  "Returns a newly created ROOM based on BLUEPRINT.
Prerenders floor and adds fixed blocks."
  (make-instance 'room :blueprint blueprint))

(defmethod initialize-instance :after ((room room) &key)
  (with-slots (wall-objects floor-objects ceiling-objects) room
    (setf wall-objects (make-hash-table)
          floor-objects (make-hash-table)
          ceiling-objects (make-hash-table)))

  ;; XXX deal with physics constants here.
  (fetus:use-image-palette (image-of (aref *tiles* 1)))
  (paint-floor room)
  (dolist (block (blocks-of (blueprint-of room)))
    (give-block-sprite room block)))

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
  (array-dimension (floor-of (blueprint-of room)) 1))

(defmethod depth-of ((room room))
  (array-dimension (floor-of (blueprint-of room)) 0))


(defmethod paint ((room room) (camera camera))
  (fetus:fill-background (fetus:get-local-color (backdrop-of room)))
  (with-slots (x y height) camera
    (with-slots (floor-buffer) room
     (fetus:blit-image floor-buffer
                       (- x (half (fetus:surface-w floor-buffer)))
                       y)))
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

    (with-slots (floor-buffer) room
      (when (slot-boundp room 'floor-buffer)
        (fetus:free-image floor-buffer))
      (setf floor-buffer (fetus:new-image-buffer h-max v-max))
      (fetus:fill-background 0 floor-buffer)
      (paint-floor-internal (floor-of (blueprint-of room))
                            floor-buffer
                            h-extent h-offs
                            v-extent v-offs))))

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

(defun make-wall-object (x z)
  (make-instance 'wall-object
                 :position #I((* x +tile-size+) 0 (* z +tile-size+))
                 :box (make-box :position #I(0 0 0)
                                :dimensions #I(+tile-size+
                                               *room-highest-point*
                                               +tile-size+))))
(defun make-floor-object (x z)
  (make-instance 'floor-object
                 :position #I((* x +tile-size+) -16
                              (* z +tile-size+))
                 :box (make-box :position #I(0 0 0)
                                :dimensions #I(+tile-size+
                                               16
                                               +tile-size+))))
(defun make-ceiling-object (x z)
  (make-instance 'ceiling-object
                 :position #I((* x +tile-size+)
                              *room-highest-point*
                              (* z +tile-size+))
                 :box
                 (make-box :position #I(0 0 0)
                           :dimensions #I(+tile-size+
                                          64
                                          +tile-size+))))

(defun get-room-object (room type x z)
  (asif (gethash (position-hash-key x z) (slot-value room type))
        it
        (setf it (ecase type
                   (wall-objects (make-wall-object x z))
                   (floor-objects (make-floor-object x z))
                   (ceiling-objects (make-ceiling-object x z))))))

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
