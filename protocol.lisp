(in-package :aequus-noctis)

(defgeneric paint (what where)
  (:documentation "Render WHAT on the current display, in the context
  of WHERE."))

(defgeneric update (who where time-elapsed)
  (:documentation "Updates a game object WHO, within the frame of
  reference of WHERE, by a time step of TIME-ELAPSED."))

(defgeneric notify (who where what &key &allow-other-keys)
  (:documentation "Sends a game event message."))

(defgeneric width-of (object))
(defgeneric height-of (object))
(defgeneric depth-of (object))
