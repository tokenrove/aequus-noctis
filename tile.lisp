(in-package :aequus-noctis)

(defconstant +tile-size+ 64)

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


(defclass room-tile ()
  ((archetype :accessor archetype-of)
   (image :accessor image-of)))

(defun initialize-tiles ()
  ;; go through  each tile, load image.
  (do ((i 0 (1+ i))
       (archetype *tile-archetypes* (cdr archetype)))
      ((null archetype))
    (let ((tile (make-instance 'room-tile)))
      (setf (archetype-of tile) (car archetype))
      (setf (image-of tile)
            (when (assoc :image (cdar archetype))
              (fetus:load-image (cadr (assoc :image (cdar archetype)))
                                t)))
      (setf (aref *tiles* i) tile))))


