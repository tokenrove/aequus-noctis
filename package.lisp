
(defpackage :aequus-noctis
  (:nicknames :equinox)
  (:use :cl :anaphora :game-fetus-alpha)
  (:export #:sinkf
	   #:clampf
	   #:half
	   #:quarter
	   #:iso-project-point
	   ;; XXX shouldn't be here
	   #:*camera*
	   #:*camera-follow*
	   #:*magic-exit-hack*
	   #:*exit-counter*
	   ;; actor
	   #:initialize-actor-data
	   #:create-actor-manager
	   #:spawn-actor-from-archetype
	   #:update-all-actors
	   #:isometric-sprite-cmp
	   #:check-room-change
	   ;; generic actor handlers
	   #:create-do-nothing-handler
	   #:create-human-input-handler
	   #:create-floating-block-handler
	   #:create-monster-handler
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
	   #:load-room
	   #:room-redraw
	   ;; room-editor
	   #:room-editor))