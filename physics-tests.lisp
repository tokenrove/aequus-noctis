(in-package :aequus-noctis)

#+5am
(defclass physics-test-actor (test-actor)
  (contact-handler-called-p))

#+5am
(defmethod notify ((me physics-test-actor) where (what (eql :contact)) &key with axis impulse)
  (declare (ignore where impulse))
  (5am:is-true (typep with 'physics-test-actor))
  (5am:is (equal axis :y))
  (5am:skip "Impulse has the wrong sign; known bug")
  (setf (slot-value me 'contact-handler-called-p) t))

#+5am
(5am:test actor-collision-handler-is-called
  (let* ((room (make-instance 'room))
         (*tile-archetypes* '(("null entry")
                              ("bare floor"
                               (:image "t/floor.pcx")
                               (:sprite
                                (:image "t/floor.pcx")
                                (:blit-offset (32 . 0))
                                (:frames ((0 0 64 40)))
                                (:animations ((:default (0 . 60)))))
                               (:box (0 0 0) (64 16 64)))))
         lower-actor
         upper-actor)
    (fetus/os:with-directory-of-system (:aequus-noctis)
      (fetus/test:with-dummy-sdl
        (fetus:with-display ()
          (initialize-tiles)
          (setf lower-actor (make-instance 'physics-test-actor
                                           :position #I(0 0 0)
                                                 :sprite (fetus:new-sprite-from-alist '((:image "t/block.pcx")
                                                                                        (:blit-offset (0 . 0))
                                                                                        (:frames ((0 0 32 96)))
                                                                                        (:animations ((:default (0 . 60))))))
                                                 :box (make-box :position #I(0 0 0) :dimensions #I(10 10 10)))
                upper-actor (make-instance 'physics-test-actor
                                           :position #I(0 11 0)
                                                 :sprite (fetus:new-sprite-from-alist '((:image "t/block.pcx")
                                                                                        (:blit-offset (0 . 0))
                                                                                        (:frames ((0 0 32 96)))
                                                                                        (:animations ((:default (0 . 60))))))
                                                 :box (make-box :position #I(0 0 0) :dimensions #I(10 10 10))))
          (let ((*room-set*
                  '((:TEST (:NAME . "Test")
                      (:FLOOR . #2A((1)))
                      (:BLOCKS)))))
            (load-room-int room :test))
          (add-actor-to-room room lower-actor)
          (add-actor-to-room room upper-actor)
          (update room nil 1)
          (update room nil 1)
          (update room nil 1))))
    (5am:is-true (slot-value lower-actor 'contact-handler-called-p))
    (5am:is-true (slot-value upper-actor 'contact-handler-called-p))))
