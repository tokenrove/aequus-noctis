;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem aequus-noctis
  :description "Game engine for primitive 2D isometric games"
  :author "Julian Squires <julian@cipht.net>"
  :license "GPL-3"
  :depends-on (:game-fetus-alpha :anaphora :fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :unit :aequus-noctis)))
  :components
  ((:file "package")
   (:file "math" :depends-on ("package"))
   (:file "actor" :depends-on ("package" "math"))
   (:file "utilities" :depends-on ("package" "math" "actor"))
   (:file "room" :depends-on ("package" "actor" "math"))
   (:file "physics" :depends-on ("actor" "room" "math"))
   (:file "room-editor" :depends-on ("room"))))
