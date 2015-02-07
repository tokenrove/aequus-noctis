
(defpackage :aequus-noctis
  (:nicknames :equinox)
  (:use :cl :anaphora :game-fetus-alpha)
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
	   #:update-actors
	   #:width-of
	   #:depth-of
	   #:border-collision
	   ;; room-editor
           #:room-editor
           #:wallhack))
