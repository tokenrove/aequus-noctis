(in-package :aequus-noctis)

#+5am
(defclass test-actor (actor)
  (handler-called-p))

#+5am
(defmethod update ((me test-actor) (where room) time-elapsed)
  (declare (ignore where time-elapsed))
  (setf (slot-value me 'handler-called-p) t))

#+5am
(5am:test actor-handler-is-called
  (let ((room (make-instance 'room))
        (*tile-archetypes* '(("null entry")
                             ("bare floor"
                              (:image "t/floor.pcx")
                              (:sprite
                               (:image "t/floor.pcx")
                               (:blit-offset (32 . 0))
                               (:frames ((0 0 64 40)))
                               (:animations ((:default (0 . 60)))))
                              (:box (0 0 0) (64 16 64)))))
        actor)
    (fetus/os:with-directory-of-system (:aequus-noctis)
      (fetus/test:with-dummy-sdl
        (fetus:with-display ()
          (setf actor (make-instance 'test-actor
                                     :type :test
                                     :position #I(0 0 0)
                                     :sprite (fetus:new-sprite-from-alist '((:image "t/block.pcx")
                                                                            (:blit-offset (0 . 0))
                                                                            (:frames ((0 0 32 96)))
                                                                            (:animations ((:default (0 . 60))))))
                                     :box (make-box :position #I(0 0 0) :dimensions #I(10 10 10))))
          (initialize-tiles)
          (let ((*room-set*
                  '((:TEST (:NAME . "Test")
                      (:FLOOR . #2A((1)))
                      (:BLOCKS) (:ACTORS) (:EXITS) (:PLAYER-SPAWN)))))
            (load-room-int room :test))
          (add-actor-to-room room actor)
          (update room nil 0.2))))
    (5am:is-true (slot-value actor 'handler-called-p))))
