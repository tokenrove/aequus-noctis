;;;
;;; Generic extensible isometric room editor.
;;;
;;; Will soon be split up to be a bit more generic, with
;;; scenario-specific hooks in demon-of-the-fall.  And fundamental
;;; editing components in game-fetus-alpha.
;;;
;;; Julian Squires <tek@wiw.org> / 2004

(in-package :aequus-noctis)

(defvar *sprite-manager* nil)
(defvar *default-font* nil)

;;;; EDITING COMPONENTS

(defun place-floor-tile (cursor tile)
  "Places a floor tile in *CURRENT-ROOM* at the supplied cursor
position.  Returns T if the floor was modified, NIL otherwise."
  (let ((orig-tile (aref (room-floor *current-room*)
			 (floor (iso-point-z cursor) +tile-size+)
			 (floor (iso-point-x cursor) +tile-size+))))
    (unless (= tile orig-tile)
      ;; Note: assumes that version in *room-set* will get modified at
      ;; the same time, because it shares structure with room-floor,
      ;; thanks to being a vector.  Not that this matters too much.
      (setf (aref (room-floor *current-room*)
		  (floor (iso-point-z cursor) +tile-size+)
		  (floor (iso-point-x cursor) +tile-size+)) tile)
      t)))

(defun remove-block (cursor)
  "Removes an immobile block from *CURRENT-ROOM* at the supplied
cursor position (if there's one there).  Returns T if the room was
modified, NIL otherwise."
  (let* ((point-list (mapcar (lambda (x y) (floor x y))
			     (iso-point-list cursor)
			     (list +tile-size+ *slice-height-increment*
				   +tile-size+)))
	 (actor (gethash point-list *room-block-actors*))
	 (archblocks (assoc :blocks (cdr (room-archetype *current-room*)))))
    (when actor
      (remhash point-list *room-block-actors*)
      (fetus:remove-sprite-from-manager *sprite-manager* (actor-sprite actor))
      (setf (cdr archblocks)
	    (delete-if #'(lambda (x) (equal point-list (cdr x)))
		       (cdr archblocks)))
      (setf (room-blocks *current-room*) (cdr archblocks))
      t)))


(defun place-block (cursor tile)
  "Places an immobile block of type TILE in *CURRENT-ROOM* at the
supplied cursor position.  If there's already something there, we
remove it.  Returns T if the room was modified, NIL otherwise."
  (let* ((block (append (list tile)
			(mapcar (lambda (x y) (floor x y))
				(iso-point-list cursor)
				(list +tile-size+ *slice-height-increment*
				      +tile-size+))))
	 (actor (gethash (cdr block) *room-block-actors*))
	 (archblocks (assoc :blocks (cdr (room-archetype *current-room*)))))
    ;; XXX really, should check if actor is same as our tile, return
    ;; NIL if so.
    (when actor (remove-block cursor))
    (if archblocks
	(push block (cdr archblocks))
	(progn
	  (push (list :blocks block) (cdr (room-archetype *current-room*)))
	  (setf archblocks (assoc :blocks
				  (cdr (room-archetype *current-room*))))))
    (setf (room-blocks *current-room*) (cdr archblocks))
    (give-block-sprite block *sprite-manager*)
    t))


(defun at-floor-level-p (cursor)
  "Returns T if the cursor's Y is at floor level, NIL otherwise."
  (= (iso-point-y cursor) *floor-slice-y*))


;;;; HIGH-LEVEL ROUTINES

(defun room-editor (room-to-edit)
  "Interactive level editor on the current display, affecting the
ROOM-TO-EDIT, which is loaded from *ROOM-SET*.  Note that the display
must already have been created with FETUS:CREATE-DISPLAY."
  (fetus:font-init)
  (setf *default-font* (fetus:load-font "other-data/pph.ttf" 18))
  (initialize-tiles)
  (setf *sprite-manager* (fetus:create-sprite-manager #'isometric-sprite-cmp))
  (load-room room-to-edit *sprite-manager* :spawn-actors-p nil)

  (do ((entry-mode :blocks)
       (slice-cursor (make-iso-point))
       (cursor-box (make-box :dimensions #I(+tile-size+
					    *floor-tile-height*
					    +tile-size+)))
       (cur-tile 1)
       (cur-spawn :apple)
       (dirty-floor-p nil)
       (unsaved-changes-p nil)
       (output-file "rooms-edit-test.sexp"))
      (nil)
    (let ((event (get-key-event)))
      ;; mode events; please excuse the silly hardcoded SDL keysyms.
      (cond ((= event (char-code #\q))
	     (when (or (not unsaved-changes-p)
		       (editor-yes-no-prompt
			*default-font*
			"Unsaved changes - really quit? (Y/N)"))
	       (return)))

	    ((= event (char-code #\a))
	     (unless (eql entry-mode :actors)
	       (setf entry-mode :actors)
	       (editor-osd-display-message "Actors mode.")))
	    ((= event (char-code #\s))
	     (unless (eql entry-mode :blocks)
	       (setf entry-mode :blocks)
	       (editor-osd-display-message "Blocks mode.")))

	    ((= event (char-code #\p))
	     (if (eql entry-mode :blocks)
		 (setf cur-tile
		       (palette-mode *tiles* #'tile-image
				     (lambda (x) (car (tile-archetype x)))))
		 (setf cur-spawn
		       (palette-mode *actor-archetypes*
				     (lambda (x) nil)
				     (lambda (x)
				       (format nil "~A" (car x)))))))

	    ((= event (char-code #\w))
	     (when (editor-yes-no-prompt *default-font*
					 "Really write changes? (Y/N)")
	       (with-open-file (stream output-file
				       :direction :output
				       :if-exists :supersede)
		 (princ ";; -*- Lisp -*-" stream)
		 (princ #\Newline stream)
		 (princ ";; This file generated by room-editor.lisp." stream)
		 (print *room-set* stream))
	       (setf unsaved-changes-p nil)
	       (editor-osd-display-message "Written to ~A." output-file)))

	    ((= event (char-code #\e))
	     (edit-exits-dialog))

	    ((= event (char-code #\r))
	     (when (editor-yes-no-prompt
		    *default-font*
		    (if unsaved-changes-p
			"Really re-read data? (you have unsaved changes!)"
			(format nil "Read map data from ~A?" output-file)))
	       (initialize-room-data output-file)
	       (load-room room-to-edit *sprite-manager* :spawn-actors-p nil)
	       (setf unsaved-changes-p t)
	       (editor-osd-display-message "Room data freshly read from ~A."
					   output-file)))

	    ((or (= event (char-code #\h))
		 (= event (char-code #\/)) ; #\? without shift.
		 (= event 282))		; F1
	     (room-editor-help))
	    ((= event (char-code #\c))
	     (let ((dest-room (change-rooms-dialog room-to-edit)))
	       (fetus:destroy-sprite-manager *sprite-manager*)
	       (setf *sprite-manager* (fetus:create-sprite-manager
				       #'isometric-sprite-cmp))
	       (load-room dest-room *sprite-manager* :spawn-actors-p nil)
	       (setf slice-cursor (make-iso-point))
	       (editor-osd-display-message "Change to room ~A."
					   dest-room)))

	    ;; slice events
	    ((= event (char-code #\-))
	     (unless (< (iso-point-y slice-cursor) 0)
	       (if (= (iso-point-y slice-cursor) 0)
		   (setf (iso-point-y slice-cursor) *floor-slice-y*)
		   (decf (iso-point-y slice-cursor)
			 *slice-height-increment*))))
	    ((= event (char-code #\=))	; #\+ without shift.
	     (unless (> (iso-point-y slice-cursor) *room-highest-point*)
	       (if (at-floor-level-p slice-cursor)
		   (setf (iso-point-y slice-cursor) 0)
		   (incf (iso-point-y slice-cursor)
			 *slice-height-increment*))))
	    ((= event 273)		; up
	     (unless (>= (ceiling (iso-point-x slice-cursor) +tile-size+)
			 (1- (room-width)))
	       (incf (iso-point-x slice-cursor) +tile-size+)))
	    ((= event 274)		; down
	     (unless (<= (iso-point-x slice-cursor) 0)
	       (decf (iso-point-x slice-cursor) +tile-size+)))
	    ((= event 275)		; right
	     (unless (<= (iso-point-z slice-cursor) 0)
	       (decf (iso-point-z slice-cursor) +tile-size+)))
	    ((= event 276)		; left
	     (unless (>= (ceiling (iso-point-z slice-cursor) +tile-size+)
			 (1- (room-depth)))
	       (incf (iso-point-z slice-cursor) +tile-size+)))

	    ;; edit events
	    ((= event (char-code #\z))
	     (if (eql entry-mode :blocks)
		 (if (at-floor-level-p slice-cursor)
		     (awhen (place-floor-tile slice-cursor cur-tile)
			    (setf dirty-floor-p t
				  unsaved-changes-p t))
		     (awhen (place-block slice-cursor cur-tile)
			    (setf unsaved-changes-p t)))
		 (format t "~&spawn!")))
	    ((= event (char-code #\x))
	     (if (eql entry-mode :blocks)
		 (if (at-floor-level-p slice-cursor)
		     (awhen (place-floor-tile slice-cursor 0)
			    (setf dirty-floor-p t
				  unsaved-changes-p t))
		     (awhen (remove-block slice-cursor)
			    (setf unsaved-changes-p t)))
		 (format t "~&despawn!")))))

      (multiple-value-bind (x y) (iso-project-point slice-cursor)
	(decf x (half (display-width)))
	(decf y (half (display-height)))
	(setf (car *camera*) (- x)
	      (cdr *camera*) (- y)))

      (maphash (lambda (id actor)
		 (declare (ignore id))
		 (update-sprite-coords (actor-sprite actor)
				       (actor-position actor)
				       actor))
	       *actor-map*)

      (when dirty-floor-p
	(paint-floor)
	(setf dirty-floor-p nil))
    (room-redraw)
    (fetus:update-all-sprites *sprite-manager*)

      (dolist (spawn (cdr (assoc :actors
				 (cdr (room-archetype *current-room*)))))
	(let ((arch (cdr (assoc (first spawn) *actor-archetypes*))))
	  (draw-debug-box (make-box
			   :position (iso-point-from-list (second spawn))
			   :dimensions
			   (iso-point-from-list (third
						 (assoc :box arch))))
			  :partial t)))

      (if (at-floor-level-p slice-cursor)
	  (setf (iso-point-y (box-dimensions cursor-box)) *floor-tile-height*)
	  (setf (iso-point-y (box-dimensions cursor-box))
		*slice-height-increment*))
      (setf (box-position cursor-box) slice-cursor)

      (if (at-floor-level-p slice-cursor)
	  (draw-top-cursor cursor-box)
	  (draw-debug-box cursor-box :partial t))

      (aif *editor-osd-message*
	   (progn
	     (draw-status-message *default-font* (cdr it)
				  100 100 140)
	     (decf (car it))
	     (when (zerop (car it))
	       (setf *editor-osd-message* nil)))
	   (draw-status-message *default-font*
				(format nil "X,Z: ~A,~A  Y: ~A  Room: ~A"
					(floor (iso-point-x slice-cursor)
					       +tile-size+)
					(floor (iso-point-z slice-cursor)
					       +tile-size+)
					(iso-point-y slice-cursor)
					(room-name *current-room*))
				40 40 80))
      (refresh-display))
  
  (fetus:destroy-font *default-font*)
  (fetus:destroy-sprite-manager *sprite-manager*))

(defun palette-mode (set image-fn name-fn)
  (do ((cursor (cons 0 0))
       (max-row (ceiling (length set) 3))
       (max-column 3))
      (nil)
    (fetus:fill-background 255)
    (dotimes (y max-row)
      (dotimes (x max-column)
	(when (< (+ x (* y max-column)) (length set))
	  (awhen (funcall image-fn (elt set (+ x (* y max-column))))
		 (fetus:blit-image it (+ 10 (* 104 x)) (+ 55 (* 70 y))))
	  (fetus:paint-string *default-font*
			      (funcall name-fn
				       (elt set
					    (+ x (* y max-column))))
			      (+ 10 (* 104 x)) (+ 60 (* 70 y)) 220 220 255))))
    (fetus:draw-rectangle (+ 8 (* 104 (car cursor)))
			  (+ 10 (* 70 (cdr cursor)))
			  84 64 63)
    (fetus:refresh-display)

    (let ((event (fetus:get-key-event)))
      ;; mode events; please excuse the silly hardcoded SDL keysyms.
      (cond ((= event 13) (return (+ (car cursor)
				     (* (cdr cursor) max-column))))
	    ((= event 273) (when (plusp (cdr cursor)) (decf (cdr cursor))))
	    ((= event 274) (if (< (cdr cursor) (1- max-row))
			       (incf (cdr cursor))
			       (setf (cdr cursor) 0)))
	    ((= event 275) (if (< (car cursor) (1- max-column))
			       (incf (car cursor))
			       (setf (car cursor) 0)))
	    ((= event 276) (when (plusp (car cursor))
			     (decf (car cursor))))))))


(defun room-editor-help ()
  (do ((cursor (cons 0 0))
       (max-row 0)
       (help-text '((50 "HELP FOR ROOM EDITOR")
		    "Don't worry.  This help will be improved soon."
		    nil
		    "c => change rooms;"
		    "a/s => actor/slice mode;"
		    "+/- => cursor higher/lower;"
		    "r/w => read/write data to disk;"
		    "p => palette; e => edit exits;"
		    "z/x => place/remove a tile;"
		    "q to quit the editor.  Return to quit this help."
		    "(Please read the README, too!)"
		    (200 "Have fun!"))))
      (nil)
    (fetus:fill-background 1)
    (do ((i 0 (1+ i))
	 (list help-text (cdr list)))
	((null list))
      (awhen (car list)
	     (paint-string
	      *default-font*
	      (if (consp it) (cadr it) it)
	      (if (consp it) (car it) 10)
	      (+ 4 (* i 19)) 255 255 255)))
    (fetus:refresh-display)

    (let ((event (fetus:get-key-event)))
      ;; mode events; please excuse the silly hardcoded SDL keysyms.
      (cond ((= event 13) (return))
	    ((= event 273) (when (plusp (cdr cursor)) (decf (cdr cursor))))
	    ((= event 274) (if (< (cdr cursor) (1- max-row))
			       (incf (cdr cursor))
			       (setf (cdr cursor) 0)))))))


;;; XXX should cause main editor to note unsaved changes.
(defun edit-exits-dialog ()
  (do ((cursor 0)
       (max-row (length (room-exits *current-room*))))
      (nil)
    (fetus:draw-filled-rectangle 8 8 (- (fetus:display-width) 16)
				 (- (fetus:display-height) 32)
				 128) ;(gfx-map-rgb 128 128 128)
    (draw-rectangle 8 8 (- (display-width) 16)
		    (- (display-height) 32) 32) ;(gfx-map-rgb 32 32 32)
    (draw-filled-rectangle 11 (+ 10 (* cursor 22)) (- (display-width) 19) 20
			   240) ;(gfx-map-rgb 240 100 50)

    (do ((i 0 (1+ i))
	 (list (room-exits *current-room*) (cdr list)))
	((null list))
      (paint-string
       *default-font*
       (format nil "~A ~A ~A" (first (car list))
	       (second (car list)) (third (car list)))
       12
       (+ 10 (* i 22)) 255 255 255))
    (paint-string *default-font*
		  "Hit N to create a new exit, D to delete an exit."
		  12 170 240 200 200)
    (paint-string *default-font*
		  "Hit 1, 2, or 3 to edit parts of an exit."
		  12 190 240 200 200)
    (refresh-display)

    (let ((event (get-key-event)))
      ;; mode events; please excuse the silly hardcoded SDL keysyms.
      (cond ((= event (char-code #\q))
	     ;; ensure room-exits and *room-set* are in sync.
	     (aif (assoc :exits (cdr (room-archetype *current-room*)))
		  (setf (cdr it) (room-exits *current-room*))
		  (push (append (list :exits) (room-exits *current-room*))
			(cdr (room-archetype *current-room*))))
	     (return))
	    ((= event 273) (when (plusp cursor) (decf cursor)))
	    ((= event 274) (if (< cursor (1- max-row))
			       (incf cursor)
			       (setf cursor 0)))
	    ((= event (char-code #\1))
	     (setf (car (first (nth cursor (room-exits *current-room*))))
		   (editor-number-prompt *default-font* "X: ")
		   (cdr (first (nth cursor (room-exits *current-room*))))
		   (editor-number-prompt *default-font* "Z: ")))
	    ((= event (char-code #\3))
	     (setf (car (third (nth cursor (room-exits *current-room*))))
		   (editor-number-prompt *default-font* "X: ")
		   (cdr (third (nth cursor (room-exits *current-room*))))
		   (editor-number-prompt *default-font* "Z: ")))
	    ((= event (char-code #\2))
	     (setf (second (nth cursor (room-exits *current-room*)))
		   (change-rooms-dialog
		    (second (nth cursor (room-exits *current-room*))))))
	    ((= event (char-code #\d))
	     (unless (<= max-row 0)
	       (setf (room-exits *current-room*)
		     (delete (nth cursor (room-exits *current-room*))
			     (room-exits *current-room*)))
	       (decf max-row)))
	    ((= event (char-code #\n))
	     (push (list (cons 0 0) (caar *room-set*) (cons 0 0))
		   (room-exits *current-room*))
	     (incf max-row))))))

(defun change-rooms-dialog (cur-room)
  (do ((cursor
	(do ((i 0 (1+ i))
	     (list *room-set* (cdr list)))
	    ((or (null list) (eql cur-room (caar list))) i)))
       (max-row (length *room-set*)))
      (nil)
    (draw-filled-rectangle 8 8 (- (display-width) 16) (- (display-height) 32)
			   128)	;(gfx-map-rgb 128 128 128)
    (draw-rectangle 8 8 (- (display-width) 16) (- (display-height) 32)
		    32) ;(gfx-map-rgb 32 32 32)
    (draw-filled-rectangle 11 (+ 10 (* cursor 22)) (- (display-width) 19) 20
			   240)		;(gfx-map-rgb 240 100 50)

    (do ((i 0 (1+ i))
	 (list *room-set* (cdr list)))
	((null list))
      (paint-string
       *default-font*
       (format nil "~A: ~A" (caar list) (cdr (assoc :name (cdar list))))
       12
       (+ 10 (* i 22)) 255 255 255))
    (paint-string *default-font*
		  "Select a room with [enter]"
		  12 170 240 200 200)
    (paint-string *default-font*
		  "Hit N to create a new room."
		  12 190 240 200 200)
    (refresh-display)

    (let ((event (fetus:get-key-event)))
      ;; mode events; please excuse the silly hardcoded SDL keysyms.
      (cond ((= event 13) (return (car (nth cursor *room-set*))))
	    ((= event 273) (when (plusp cursor) (decf cursor)))
	    ((= event 274) (if (< cursor (1- max-row))
			       (incf cursor)
			       (setf cursor 0)))
	    ((= event (char-code #\n))
	     (let* ((name (intern (editor-string-prompt *default-font*
							"Symbol name: "
							:symbol-mode t)
				  :keyword))
		    (real-name (editor-string-prompt *default-font*
						     "Real name: "))
		    (width (editor-number-prompt *default-font* "width: "))
		    (depth (editor-number-prompt *default-font* "depth: ")))
	       (push `(,(prin1 name)
		       (:name . ,real-name)
		       (:floor . ,(make-array (list width depth)
					      :initial-element 0)))
		     *room-set*)
	       (return name)))))))


(defvar *editor-osd-message* nil)

(defun editor-osd-display-message (&rest arguments)
  (setf *editor-osd-message*
	(cons 1 (apply #'format (append (list nil) arguments)))))
