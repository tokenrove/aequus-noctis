;; -*- Lisp -*-

(defpackage #:aequus-noctis-system (:use #:cl #:asdf))
(in-package #:aequus-noctis-system)

(defsystem aequus-noctis
  :depends-on (:game-fetus-alpha :anaphora)
  :components
  ((:file "package")
   (:file "math" :depends-on ("package"))
   (:file "actor" :depends-on ("package" "math"))
   (:file "utilities" :depends-on ("package" "math" "actor"))
   (:file "room" :depends-on ("package" "actor" "math"))
   (:file "physics" :depends-on ("actor" "room" "math"))
   (:file "room-editor" :depends-on ("room"))))
