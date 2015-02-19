
(defpackage :aequus-noctis
  (:nicknames :equinox)
  (:use :cl :anaphora)
  ;; Protocol
  (:export #:paint
           #:update
           #:notify)
  ;; Camera
  (:export #:camera
           #:make-camera
           #:follow)
  (:shadow #:room)
  (:export #:sinkf
	   #:clampf
	   #:half
	   #:quarter
           #:iso-project-point
           #:iso-point-component
           #:make-iso-point
           #:iso-point-x
           #:iso-point-y
           #:iso-point-z
           ;; actor
           #:actor
           #:sprite-of
           #:facing-of
           #:box-of
	   #:initialize-actor-data
	   #:create-actor-manager
	   #:spawn-actor-from-archetype
	   #:isometric-sprite-cmp
	   #:check-room-change
           ;; physics
	   #:update-physics
           #:penetrating-p
           #:position-of
           #:velocity-of
           #:contact-surface-of
           #:apply-impulse
	   ;; room
	   #:initialize-tiles
	   #:initialize-room-data
           #:make-room
           #:blueprint-from-alist
           #:add-actor-to-room
           #:room
           #:blueprint
           #:blueprint-of
           #:width-of
           #:depth-of))

(defpackage :aequus-noctis/editor
  (:use :cl :anaphora :aequus-noctis)
  (:shadow #:room)
  (:export #:room-editor))

(in-package :aequus-noctis)
(5am:def-suite unit :description "Fast-running tests")
(5am:def-suite integration :description "Big or slow non-interactive tests")
(5am:in-suite unit)
