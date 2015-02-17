(in-package :aequus-noctis)

#+5am
(fetus/test:define-screencap-comparison-test (basic-room-rendering-test :suite unit :system :aequus-noctis)
  (let* ((*tile-archetypes* '(("null entry")
                              ("bare floor"
                               (:image "t/floor.pcx")
                               (:sprite
                                (:image "t/floor.pcx")
                                (:blit-offset (32 . 0))
                                (:frames ((0 0 64 40)))
                                (:animations ((:default (0 . 60)))))
                               (:box (0 0 0) (64 16 64)))
                              ("bare block"
                               (:image "t/block.pcx")
                               (:sprite
                                (:image "t/block.pcx")
                                (:blit-offset (32 . 0))
                                (:frames ((0 0 64 64)))
                                (:animations ((:default (0 . 60)))))
                               (:box (0 0 0) (64 32 64)))
                              ("grey wall center"
                               (:image "t/greytilef.pcx")
                               (:sprite
                                (:image "t/greytilef.pcx")
                                (:blit-offset (32 . 8))
                                (:frames ((0 0 64 144)))
                                (:animations ((:default (0 . 60)))))
                               (:box (0 0 0) (64 72 64)))
                              ("grey wall left"
                               (:image "t/greytilel.pcx")
                               (:sprite
                                (:image "t/greytilel.pcx")
                                (:blit-offset (32 . 8))
                                (:frames ((0 0 64 144)))
                                (:animations ((:default (0 . 60)))))
                               (:box (0 0 0) (64 72 64)))
                              ("grey wall right"
                               (:image "t/greytiler.pcx")
                               (:sprite
                                (:image "t/greytiler.pcx")
                                (:blit-offset (32 . 8))
                                (:frames ((0 0 64 144)))
                                (:animations ((:default (0 . 60)))))
                               (:box (0 0 0) (64 72 64)))))
         (room (make-instance 'room))
         (*room-set*
           '((:TEST-ROOM-A (:NAME . "Test Room A")
              (:FLOOR
               . #2A((0 0 0 1 0 0 0 0)
                     (0 0 1 1 1 0 0 0)
                     (0 1 1 1 1 1 3 0)
                     (0 0 1 1 1 0 0 0)
                     (0 0 1 1 1 0 0 0)
                     (1 1 1 1 1 1 1 5)
                     (1 1 1 1 1 1 1 3)
                     (4 3 1 1 1 3 3 0)
                     (0 0 4 3 3 0 0 0)))
              (:BLOCKS (2 4 0 4) (2 3 0 3) (1 4 1 4)))))
         (camera (make-camera (fetus:display-width) (fetus:display-height))))
    (initialize-tiles)
    (load-room-int room :test-room-a)
    (follow camera (aref (blocks-of room) 0))
    (update camera nil 1.)
    (paint room camera)))
