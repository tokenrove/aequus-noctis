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

;;; XXX the idea of player-spawn, exits, and name are going to be moved to
;;; the scenario logic.
(defclass room ()
  ((floor :accessor floor-of)
   (blocks :accessor blocks-of)
   (actors :accessor room-actors)
   (archetype :accessor archetype-of)
   (name :accessor room-name))
  (:documentation "ROOM encapsulates the concept of a location; a
floor, fixed blocks (set), and actors."))

(defun load-room-int (room name sprite-manager &key (spawn-actors-p t))
  "Loads the named room from *ROOM-SET*, into *CURRENT-ROOM*.
Prerenders floor, adds fixed blocks to SPRITE-MANAGER, and optionally
 (based on SPAWN-ACTORS-P) spawns actors associated with room."
  (setf *wall-objects* (make-hash-table)
	*floor-objects* (make-hash-table)
	*ceiling-objects* (make-hash-table))
  (let ((archetype (assoc name *room-set*)))
    (assert archetype () "Couldn't find room ~A." name)
    (setf (floor-of room) (cdr (assoc :floor (cdr archetype)))
	  (blocks-of room) (cdr (assoc :blocks (cdr archetype)))
	  (room-name room) (cdr (assoc :name (cdr archetype)))
	  (archetype-of room) archetype
	  (room-actors room) nil)
    (when spawn-actors-p
      (dolist (actor (cdr (assoc :actors (cdr archetype))))
	(push (spawn-actor-from-archetype (first actor)
					  (iso-point-from-list (second actor))
					  sprite-manager)
	      (room-actors room))))
    ;; XXX deal with physics constants here.
    (fetus:use-image-palette (image-of (aref *tiles* 1)))

    (paint-floor room)
    (setf *room-block-actors* (make-hash-table :test 'equal))
    (dolist (block (blocks-of room))
      (give-block-sprite block sprite-manager))
    room))

(defun width-of (room)
  (array-dimension (floor-of room) 1))

(defun depth-of (room)
  (array-dimension (floor-of room) 0))


(defmethod redraw ((room room))
  (fetus:fill-background 65)            ; XXX genericize
  (fetus:blit-image *floor-buffer*
                    (- (car *camera*) (half (fetus:surface-w *floor-buffer*)))
                    (+ (cdr *camera*) (half (fetus:surface-h *floor-buffer*))))
  ;; update sprites
  (maphash (lambda (key actor)
	     (declare (ignore key))
	     (update-sprite-coords
	      (sprite-of actor)
	      (position-of actor)
	      actor))
	   *room-block-actors*))


(defmethod update ((room room) where time-elapsed)
  "Update collisions, physics, and handlers for all actors registered
with the actor manager."
  ;; WHERE in the context of rooms is the PLAY-SESSION, but we don't
  ;; care about that here.
  (declare (ignore where))
  (maphash (lambda (id actor)
             (update-physics actor room time-elapsed)
	     (ensure-no-penetrations id actor)
	     ;; XXX update contact handlers
	     ;; XXX camera
	     (update-sprite-coords (sprite-of actor)
				   (position-of actor)
                                   actor)
             (update actor room time-elapsed))
	   *actor-map*))

;;;; FLOORS

#+equinox:wallhack(defvar *wall-image* (load-image "art/mansion/walls/greytile.pcx"))

(defun paint-floor (room)
  "function PAINT-FLOOR

Paint floor tiles according to room.  Tiles at +TILE-SIZE+ intervals;
paints from back to front."

  ;; Draw order is from bottom-right of the array (furthest away from
  ;; the camera).
  (let* ((h-extent (width-of room))
	 (v-extent (depth-of room))
	 (h-offs 0) (v-offs 0) (h-max 0) (v-max 0))

    (setf h-max (+ (* h-extent 64) (* v-extent 32))) ;; XXX constants
    (setf v-max (+ (* h-extent 20) (* v-extent 20)))
    (incf h-max 64)
    (incf v-max 64)
    (setf h-offs (half h-max))
    (setf v-offs (half v-max))
    
    ;; blit offset
    (decf v-offs 8)

    (when *floor-buffer*
      (fetus:free-image *floor-buffer*))
    (setf *floor-buffer* (fetus:new-image-buffer h-max v-max))
    (fetus:fill-background 0 *floor-buffer*)

    #+equinox:wallhack(paint-walls-internal (floor-of room)
			  *floor-buffer* 
			  h-extent h-offs
			  v-extent v-offs)
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
			  (iso-point-y pt) -16
			  (iso-point-z pt) (* +tile-size+ z))
		 (multiple-value-bind (u v)
		     (iso-project-point pt)
		   (let* ((sprite (sprite-of (archetype-of tile)))
			  (blit-offset (cadr (assoc :blit-offset sprite))))
		     (decf u (car blit-offset))
		     (decf v (cdr blit-offset))
		     (incf u h-offs)
		     (incf v v-offs)
                     (fetus:blit-image (image-of tile) u v
                                       :destination buffer)))
		 ;;else if (and (> x 0) (> z 0)) do
		 #+equinox:wallhack(setf (iso-point-x pt) (* +tile-size+ x)
		       (iso-point-y pt) -16
		       (iso-point-z pt) (* +tile-size+ z))
		 #+equinox:wallhack(multiple-value-bind (u v)
		     (iso-project-point pt)
		   (decf u 32)
		   ;;(decf v (cdr blit-offset))
		   (incf u h-offs)
		   (incf v v-offs)
		   (blit-image *wall-image* u v :src-rect '(0 0 64 144) :destination buffer)))))

#+equinox:wallhack(defun paint-walls-internal (floor buffer h-extent h-offs v-extent v-offs)
  (loop for z from (1- v-extent) downto 0
	with pt = (make-iso-point)
	with x = (1- h-extent)
	do
	(setf (iso-point-x pt) (* +tile-size+ x)
	      (iso-point-y pt) -16
	      (iso-point-z pt) (* +tile-size+ z))
	(multiple-value-bind (u v)
	    (iso-project-point pt)
	  (decf u 32)
	  ;;(decf v (cdr blit-offset))
	  (incf u h-offs)
	  (incf v v-offs)
	  (blit-image *wall-image* u v :src-rect '(0 0 64 144) :destination buffer)))
  (loop for x from (1- h-extent) downto 0
	with pt = (make-iso-point)
	with z = (1- v-extent)
	do
	(setf (iso-point-x pt) (* +tile-size+ x)
	      (iso-point-y pt) -16
	      (iso-point-z pt) (* +tile-size+ z))
	(multiple-value-bind (u v)
	    (iso-project-point pt)
	  (decf u 32)
	  ;;(decf v (cdr blit-offset))
	  (incf u h-offs)
	  (incf v v-offs)
	  (blit-image *wall-image* u v :src-rect '(0 0 64 144) :destination buffer))))


(defmethod sprite-of ((archetype list))
  (cdr (assoc :sprite (cdr archetype))))

(defun position-hash-key (x z)
  (complex x z))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *wall-objects* (make-hash-table)))
(defun make-wall-object (x z)
  (let ((objects *wall-objects*))
    (unless (gethash (position-hash-key x z) objects)
      (let ((wall (make-instance 'actor :type :wall
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
      (let ((floor (make-instance 'actor :type :floor)))
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
      (let ((ceiling (make-instance 'actor :type :ceiling)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *room-block-actors*))

(defun give-block-sprite (block sprite-manager)
  (let* ((arch (cdr (archetype-of (aref *tiles* (first block)))))
	 (actor (make-slice-object arch (second block)
				   (third block) (fourth block)))
	 (sprite (fetus:new-sprite-from-alist (cdr (assoc :sprite arch)))))
    (update-sprite-coords
     sprite
     (make-iso-point :x (* (second block) +tile-size+)
		     :y (* (third block) *slice-height-increment*)
		     :z (* (fourth block) +tile-size+))
     actor)
    (setf (sprite-of actor) sprite)
    (fetus:add-sprite-to-manager sprite-manager sprite)
    (setf (gethash (cdr block) *room-block-actors*) actor)))

(defun make-slice-object (archetype x y z)
  (let ((block (make-instance 'actor :type :block)))
    (setf (position-of block) #I((* x +tile-size+)
				 (* y *slice-height-increment*)
				 (* z +tile-size+)))
    (destructuring-bind ((x y z) (w h d)) (cdr (assoc :box archetype))
      (setf (box-of block)
	    (make-box :position (make-iso-point :x x :y y :z z)
		      :dimensions (make-iso-point :x w :y h :z d))))
    block))
