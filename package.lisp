
(defpackage :aequus-noctis
  (:nicknames :equinox)
  (:use :cl :anaphora)
  ;; Protocol
  (:export #:paint
           #:update
           #:notify)
  ;; Camera
  (:export #:make-camera
           #:follow)
  (:shadow #:room)
  (:export #:sinkf
	   #:clampf
	   #:half
	   #:quarter
	   #:iso-project-point
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
           #:room
           #:width-of
	   #:depth-of
           #:wallhack))

(defpackage :aequus-noctis/editor
  (:use :cl :anaphora :aequus-noctis)
  (:shadow #:room)
  (:export #:room-editor))

(in-package :aequus-noctis)
(5am:def-suite unit :description "Fast-running tests")
(5am:def-suite integration :description "Big or slow non-interactive tests")
(5am:in-suite unit)
