
(defpackage :aequus-noctis
  (:nicknames :equinox)
  (:use :cl :anaphora)
  ;; Protocol
  (:export #:paint
           #:update
           #:notify)
  (:shadow #:room)
  (:export #:sinkf
	   #:clampf
	   #:half
	   #:quarter
	   #:iso-project-point
	   ;; XXX shouldn't be here
	   #:update-camera
	   ;; actor
	   #:actor
	   #:initialize-actor-data
	   #:create-actor-manager
	   #:spawn-actor-from-archetype
	   #:isometric-sprite-cmp
	   #:check-room-change
	   ;; generic actor handlers
	   #:create-do-nothing-handler
	   #:create-human-input-handler
	   #:create-floating-block-handler
	   #:create-monster-handler
	   #:create-key-handler
	   #:pushable-block-handler
	   #:player-contact-handler
	   #:loot-contact-handler
	   #:monster-contact-handler
	   ;; physics
	   #:update-physics
	   #:penetrating-p
	   ;; room
	   #:initialize-tiles
	   #:initialize-room-data
	   #:load-room-int
	   #:redraw
	   #:room
           #:width-of
	   #:depth-of
	   #:border-collision
	   ;; room-editor
           #:room-editor
           #:wallhack))

(in-package :aequus-noctis)
(5am:def-suite unit :description "Fast-running tests")
(5am:def-suite integration :description "Big or slow non-interactive tests")
(5am:in-suite unit)
