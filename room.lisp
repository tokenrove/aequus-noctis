;;;
;;; room.lisp -- Room management for Equinox-ish games.
;;;
;;; Author: Julian Squires <tek@wiw.org> / 2004
;;;

(in-package :aequus-noctis)

(defconstant +tile-size+ 64)

(defparameter *room-lowest-point* 0)
(defparameter *room-highest-point* 256)
(defparameter *floor-slice-y* -16)
(defparameter *slice-height-increment* 32)
(defparameter *floor-tile-height* 16)

;; Remember that a change made to this array needs to be followed by a
;; call to initialize-tiles.
;; Note that floors can't be animated, but blocks can.
(defparameter *tile-archetypes*
  '(("null entry")
    ("bare floor"
     (:image "ret-data/floor.pcx")
     (:sprite
      (:image "ret-data/floor.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 16 64)))
    ("checkered floor"
     (:image "ret-data/fl-check.pcx")
     (:sprite
      (:image "ret-data/fl-check.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 16 64)))
    ("carpet floor"
     (:image "ret-data/fl-carpt.pcx")
     (:sprite
      (:image "ret-data/fl-carpt.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 16 64)))
    ("fancy floor"
     (:image "ret-data/fl-fancy.pcx")
     (:sprite
      (:image "ret-data/fl-fancy.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 16 64)))
    ("marble floor"
     (:image "ret-data/fl-marb.pcx")
     (:sprite
      (:image "ret-data/fl-marb.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 16 64)))
    ("Roy's floor?"
     (:image "ret-data/fl-roy.pcx")
     (:sprite
      (:image "ret-data/fl-roy.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 16 64)))
    ("bare block"
     (:image "ret-data/block.pcx")
     (:sprite
      (:image "ret-data/block.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 64)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 32 64)))
    ("cushion block"
     (:image "ret-data/bl-cushi.pcx")
     (:sprite
      (:image "ret-data/bl-cushi.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 64)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 32 64)))
    ("hedge block"
     (:image "ret-data/bl-hedge.pcx")
     (:sprite
      (:image "ret-data/bl-hedge.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 64)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 32 64)))
    ("decorative table"
     (:image "ret-data/dc-table.pcx")
     (:sprite
      (:image "ret-data/dc-table.pcx")
      (:blit-offset (32 . 0))
      (:frames ((0 0 64 40)))
      (:animations ((:default (0 . 60)))))
     (:box (0 0 0) (64 16 64)))))

(defparameter *tiles* (make-array (list (length *tile-archetypes*))))

(defvar *floor-buffer* nil)
;;; Note that the actors list gets eval'd -- this allows random
;;; placement and other fun.
(defvar *room-set* nil)
(defvar *current-room*)


(defclass room-tile ()
  ((archetype :accessor tile-archetype)
   (image :accessor tile-image)))

(defun initialize-tiles ()
  ;; go through  each tile, load image.
  (do ((i 0 (1+ i))
       (archetype *tile-archetypes* (cdr archetype)))
      ((null archetype))
    (let ((tile (make-instance 'room-tile)))
      (setf (tile-archetype tile) (car archetype))
      (setf (tile-image tile)
	    (when (assoc :image (cdar archetype))
	      (load-image (cadr (assoc :image (cdar archetype)))
			  t)))
      (setf (aref *tiles* i) tile))))


(defun initialize-room-data (&optional (rooms-file "rooms.sexp"))
  (with-open-file (stream rooms-file)
    (setf *room-set* (read stream))))

;;; XXX the idea of player-spawn, exits, and name are going to be moved to
;;; the scenario logic.
(defclass iso-room ()
  ((floor :accessor room-floor)
   (blocks :accessor room-blocks)
   (exits :accessor room-exits)
   (actors :accessor room-actors)
   (archetype :accessor room-archetype)
   (name :accessor room-name)
   (player-spawn :accessor room-player-spawn))
;   )
  (:documentation "ISO-ROOM encapsulates the concept of a location; a
floor, fixed blocks (set), and actors.  This class is called ISO-ROOM
to avoid conflict with the CL symbol ROOM, but all of its associated
methods drop the ISO qualifier."))

(defun load-room (name sprite-manager &key (spawn-actors-p t))
  "Loads the named room from *ROOM-SET*, into *CURRENT-ROOM*.
Prerenders floor, adds fixed blocks to SPRITE-MANAGER, and optionally
 (based on SPAWN-ACTORS-P) spawns actors associated with room."
  (setf *wall-objects* (make-hash-table)
	*floor-objects* (make-hash-table)
	*ceiling-objects* (make-hash-table))
  (let ((room (make-instance 'iso-room))
	(archetype (assoc name *room-set*)))
    (assert archetype () "Couldn't find room ~A." name)
    (setf (room-floor room) (cdr (assoc :floor (cdr archetype)))
	  (room-exits room) (cdr (assoc :exits (cdr archetype)))
	  (room-blocks room) (cdr (assoc :blocks (cdr archetype)))
	  (room-name room) (cdr (assoc :name (cdr archetype)))
	  (room-archetype room) archetype
	  (room-actors room) nil)
    (setf *current-room* room)
    (setf (room-player-spawn room)
	  (cadr (assoc :player-spawn (cdr archetype))))
    (when spawn-actors-p
      (dolist (actor (cdr (assoc :actors (cdr archetype))))
	(push (spawn-actor-from-archetype (first actor)
					  (iso-point-from-list (second actor))
					  sprite-manager)
	      (room-actors room))))
    ;; XXX deal with physics constants here.
    (use-image-palette (tile-image (aref *tiles* 1)))

    (paint-floor)
    (setf *room-block-actors* (make-hash-table :test 'equal))
    (dolist (block (room-blocks room))
      (give-block-sprite block sprite-manager))
    room))

(defun room-width (&optional (room *current-room*))
  (array-dimension (room-floor room) 1))

(defun room-depth (&optional (room *current-room*))
  (array-dimension (room-floor room) 0))


(defun room-redraw ()
  (fill-background 65)
  (blit-image *floor-buffer*
	      (- (car *camera*) (half (surface-w *floor-buffer*)))
	      (+ (cdr *camera*) (half (surface-h *floor-buffer*))))
  ;; draw exits
  (dolist (exit (room-exits *current-room*))
    (when (consp (first exit))
      (when (minusp (1- (caar exit)))
	(draw-exit exit t nil))
      (when (minusp (1- (cdar exit)))
	(draw-exit exit nil nil))
      (when (= (caar exit) (1- (room-width)))
	(draw-exit exit t t))
      (when (= (cdar exit) (1- (room-depth)))
	(draw-exit exit nil t))))
  ;; update sprites
  (maphash (lambda (key actor)
	     (declare (ignore key))
	     (update-sprite-coords
	      (actor-sprite actor)
	      (actor-position actor)
	      actor))
	   *room-block-actors*))


;; Draws triangles.  These could be more efficiently drawn as sprites,
;; but for the moment it doesn't matter too much.
(defun draw-exit (exit ex great)
  (multiple-value-bind (x1 y1)
      (iso-project-point #I((* +tile-size+ (+ (caar exit)
					      (if (and ex great) 1 0))) 0
			    (* +tile-size+ (+ (cdar exit)
					      (if (and (not ex) great) 1 0)))))
    (multiple-value-bind (x2 y2)
	(iso-project-point #I((* +tile-size+ (+ (caar exit)
						(if (and ex (not great)) 0 1)))
			      0
			      (* +tile-size+ (+ (cdar exit)
						(if (or ex great) 1 0)))))
      (multiple-value-bind (x3 y3)
	  (iso-project-point
	   #I((+ (* +tile-size+ (caar exit))
		 (cond ((and ex (not great)) (- (half +tile-size+)))
		       ((not ex) (half +tile-size+))
		       (t (+ +tile-size+ (half +tile-size+)))))
	      0
	      (+ (* +tile-size+ (cdar exit))
		 (cond (ex (half +tile-size+))
		       ((not great) (- (half +tile-size+)))
		       (t (+ +tile-size+ (half +tile-size+)))))))

	(incf x1 (car *camera*))
	(incf x2 (car *camera*))
	(incf x3 (car *camera*))
	(incf y1 (cdr *camera*))
	(incf y2 (cdr *camera*))
	(incf y3 (cdr *camera*))
	(draw-filled-triangle (list x1 x2 x3) (list y1 y2 y3) 150)
	(draw-triangle (list x1 x2 x3) (list y1 y2 y3) 64)))))


;;;; FLOORS

(defun paint-floor ()
  "function PAINT-FLOOR

Paint floor tiles according to room.  Tiles at +TILE-SIZE+ intervals;
paints from back to front."

  ;; Draw order is from bottom-right of the array (furthest away from
  ;; the camera).
  (let* ((floor (room-floor *current-room*))
	 (h-extent (array-dimension floor 1))
	 (v-extent (array-dimension floor 0))
	 (h-offs 0) (v-offs 0) (h-max 0) (v-max 0))

    (setf h-max (+ (* h-extent 32) (* v-extent 32)))
    (setf v-max (+ (* h-extent 20) (* v-extent 20)))
    (incf h-max 64)
    (incf v-max 64)
    (setf h-offs (half h-max))
    (setf v-offs (half v-max))
    
    ;; blit offset
    (decf v-offs 8)

    (when *floor-buffer*
      (free-image *floor-buffer*))
    (setf *floor-buffer* (new-image-buffer h-max v-max))
    (fill-background 0 *floor-buffer*)

    (paint-floor-internal floor *floor-buffer* 
			  h-extent h-offs
			  v-extent v-offs)))

(defun paint-floor-internal (floor buffer h-extent h-offs v-extent v-offs)
  (do ((z (1- v-extent) (1- z))
       (pt (make-iso-point)))
      ((< z 0))
    (do ((x (1- h-extent) (1- x)))
	((< x 0))
      (let ((tile (aref floor z x)))
	(when (plusp tile)
	  (setf (iso-point-x pt) (* +tile-size+ x)
		(iso-point-y pt) -16
		(iso-point-z pt) (* +tile-size+ z))
	  (multiple-value-bind (u v) (iso-project-point pt)
	    (let* ((sprite (cdr (assoc :sprite (cdr (tile-archetype
						     (aref *tiles* tile))))))
		   (blit-offset (cadr (assoc :blit-offset sprite))))
	      (decf u (car blit-offset))
	      (decf v (cdr blit-offset))
	      (incf u h-offs)
	      (incf v v-offs)
	      (blit-image (tile-image (aref *tiles* tile)) u v
			  :destination buffer))))))))


(defun position-hash-key (x z)
  (+ x (* z (1+ (max (room-depth) (room-width))))))

(defvar *wall-objects* (make-hash-table))
(defun make-wall-object (x z)
  (let ((objects *wall-objects*))
    (unless (gethash (position-hash-key x z) objects)
      (let ((wall (make-instance 'actor)))
	(setf (actor-position wall) #I((* x +tile-size+) 0
				       (* z +tile-size+)))
	(setf (actor-box wall)
	      (make-box :position #I(0 0 0)
			:dimensions #I(+tile-size+
				       *room-highest-point*
				       +tile-size+)))
	(setf (gethash (position-hash-key x z) objects) wall)))
    (gethash (position-hash-key x z) objects)))

(defvar *floor-objects* (make-hash-table))
(defun make-floor-object (x z)
  (let ((objects *floor-objects*))
    (unless (gethash (position-hash-key x z) objects)
      (let ((floor (make-instance 'actor)))
	(setf (actor-position floor) #I((* x +tile-size+) -16
					(* z +tile-size+)))
	(setf (actor-box floor)
	      (make-box :position #I(0 0 0)
			:dimensions #I(+tile-size+
				       16
				       +tile-size+)))
	(setf (gethash (position-hash-key x z) *floor-objects*) floor)))
    (gethash (position-hash-key x z) objects)))


(defvar *ceiling-objects* (make-hash-table))
(defun make-ceiling-object (x z)
  (let ((objects *ceiling-objects*))
    (unless (gethash (position-hash-key x z) objects)
      (let ((ceiling (make-instance 'actor)))
	(setf (actor-position ceiling) #I((* x +tile-size+)
					  *room-highest-point*
					  (* z +tile-size+)))
	(setf (actor-box ceiling)
	      (make-box :position #I(0 0 0)
			:dimensions #I(+tile-size+
				       64
				       +tile-size+)))
	(setf (gethash (position-hash-key x z) objects) ceiling)))
    (gethash (position-hash-key x z) objects)))


;;;; BLOCKS

(defvar *room-block-actors*)

(defun give-block-sprite (block sprite-manager)
  (let* ((arch (cdr (tile-archetype (aref *tiles* (first block)))))
	 (actor (make-slice-object arch (second block)
				   (third block) (fourth block)))
	 (sprite (fetus:new-sprite-from-alist (cdr (assoc :sprite arch)))))
    (update-sprite-coords
     sprite
     (make-iso-point :x (* (second block) +tile-size+)
		     :y (* (third block) *slice-height-increment*)
		     :z (* (fourth block) +tile-size+))
     actor)
    (setf (actor-sprite actor) sprite)
    (fetus:add-sprite-to-manager sprite-manager sprite)
    (setf (gethash (cdr block) *room-block-actors*) actor)))

(defun make-slice-object (archetype x y z)
  (let ((block (make-instance 'actor)))
    (setf (actor-position block) #I((* x +tile-size+)
				    (* y *slice-height-increment*)
				    (* z +tile-size+)))
    (destructuring-bind ((x y z) (w h d)) (cdr (assoc :box archetype))
      (setf (actor-box block)
	    (make-box :position (make-iso-point :x x :y y :z z)
		      :dimensions (make-iso-point :x w :y h :z d))))
    block))


;;; XXX this will go elsewhere

(defun check-room-change ()
  (when (plusp *exit-counter*) (decf *exit-counter*))
  (when (and *magic-exit-hack* (zerop *exit-counter*))
    (let* ((old-y (iso-point-y (actor-position *camera-follow*)))
	   (point #I((* (caadr *magic-exit-hack*) +tile-size+)
		     old-y
		     (* (cdadr *magic-exit-hack*) +tile-size+)))
	   (room (car *magic-exit-hack*)))
      (setf *exit-counter* 61)
      (setf *magic-exit-hack* nil)
      (values room point))))