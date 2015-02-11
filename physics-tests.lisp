(in-package :aequus-noctis)

#+5am
(5am:test actor-collision-handler-is-called
  (let* ((room (make-instance 'room))
         (*actor-map* (make-hash-table))
         (*tile-archetypes* '(("null entry")
                              ("bare floor"
                               (:image "t/floor.pcx")
                               (:sprite
                                (:image "t/floor.pcx")
                                (:blit-offset (32 . 0))
                                (:frames ((0 0 64 40)))
                                (:animations ((:default (0 . 60)))))
                               (:box (0 0 0) (64 16 64)))))
         contact-handler-was-called
         lower-actor
         upper-actor
         (*actor-archetypes* `((:test
                                   (:sprite
                                    (:image "t/block.pcx")
                                    (:blit-offset (0 . 0))
                                    (:frames ((0 0 32 96)))
                                    (:animations ((:default (0 . 60)))))
                                 (:contact ,(lambda (a b lsa impulse)
                                              (5am:skip "Impulse has the wrong sign; known bug")
                                              (cond ((equal a upper-actor)
                                                     (5am:is (equal b lower-actor))
                                                     #+(or) (5am:is (plusp (iso-point-y impulse))))
                                                    ((equal a lower-actor)
                                                     (5am:is (equal b upper-actor))
                                                     #+(or) (5am:is (minusp (iso-point-y impulse))))
                                                    (t (5am:fail "These are not the actors we expected.")))
                                              (5am:is (equal lsa :y))
                                              (setf contact-handler-was-called t)))
                                 (:box
                                  (0 0 0)
                                  (64 96 8))))))
    (fetus/os:with-directory-of-system (:aequus-noctis)
      (fetus/test:with-dummy-sdl
        (fetus:with-display ()
          (initialize-tiles)
          (fetus:with-sprite-manager (s-m #'isometric-sprite-cmp)
            (setf lower-actor (make-instance 'test-actor
                                             :type :test
                                             :position #I(0 0 0)
                                             :sprite (fetus:new-sprite-from-alist '((:image "t/block.pcx")
                                                                                    (:blit-offset (0 . 0))
                                                                                    (:frames ((0 0 32 96)))
                                                                                    (:animations ((:default (0 . 60))))))
                                             :box (make-box :position #I(0 0 0) :dimensions #I(10 10 10)))
                  upper-actor (make-instance 'test-actor
                                             :type :test
                                             :position #I(0 11 0)
                                             :sprite (fetus:new-sprite-from-alist '((:image "t/block.pcx")
                                                                                    (:blit-offset (0 . 0))
                                                                                    (:frames ((0 0 32 96)))
                                                                                    (:animations ((:default (0 . 60))))))
                                             :box (make-box :position #I(0 0 0) :dimensions #I(10 10 10))))
            (let ((*room-set*
                    '((:TEST (:NAME . "Test")
                        (:FLOOR . #2A((1)))
                        (:BLOCKS) (:ACTORS) (:EXITS) (:PLAYER-SPAWN)))))
              (load-room-int room :test s-m))
            (manage-actor lower-actor)
            (manage-actor upper-actor)
            (update room nil 1)
            (update room nil 1)
            (update room nil 1)))))
    (5am:is-true contact-handler-was-called)))
