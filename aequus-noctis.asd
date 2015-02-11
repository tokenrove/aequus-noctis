;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem aequus-noctis
  :description "Game engine support for primitive 2D isometric games"
  :author "Julian Squires <julian@cipht.net>"
  :license "GPL-3"
  :depends-on (:game-fetus-alpha :anaphora :fiveam)
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :unit :aequus-noctis)))
  :components
  ((:file "package")
   (:file "protocol" :depends-on ("package"))
   (:file "math" :depends-on ("package"))
   (:file "actor" :depends-on ("package" "math" "protocol"))
   (:file "utilities" :depends-on ("package" "math" "actor"))
   (:file "tile" :depends-on ("math"))
   (:file "room" :depends-on ("actor" "tile" "protocol"))
   (:file "physics" :depends-on ("actor" "room" "math"))
   (:file "room-editor" :depends-on ("room" "utilities"))
   (:file "actor-tests" :depends-on ("actor" "room"))))
