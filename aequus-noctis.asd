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
   (:file "camera" :depends-on ("protocol" "math"))
   (:file "actor" :depends-on ("package" "math" "protocol" "camera"))
   (:file "tile" :depends-on ("math"))
   (:file "room" :depends-on ("actor" "tile" "protocol" "camera"))
   (:file "physics" :depends-on ("actor" "room" "math"))
   (:file "actor-tests" :depends-on ("actor" "room"))
   (:file "physics-tests" :depends-on ("actor-tests" "physics"))
   (:file "room-tests" :depends-on ("actor-tests")))
  :components
  ((:file "utilities" :depends-on ("package" "math"))
   (:file "room-editor" :depends-on ("room" "utilities"))))
