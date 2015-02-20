
(in-package :aequus-noctis)

(defclass camera ()
  ((x :initarg :x)
   (y :initarg :y)
   (width :accessor width-of :initarg :width)
   (height :accessor height-of :initarg :height)
   (target :accessor target-of :initarg :target
    :documentation "Something we are following, that responds to POSITION-OF"))
  (:documentation "")
  (:default-initargs :x 100 :y 140))

(defgeneric follow (follower leader))

(defun make-camera (width height)
  (make-instance 'camera :width width :height height))

(defmethod follow ((camera camera) target)
  (setf (target-of camera) target))

;;; We might want to add some physicality to camera here, using
;;; ease-in, ease-out for its movements.
(defmethod update ((camera camera) where time-delta)
  (declare (ignore where time-delta))
  (when (slot-boundp camera 'target)
    (with-slots (x y width height target) camera
      (multiple-value-bind (tx ty) (iso-project-point (position-of target))
        (setf x (- (half width) tx) y (- (half height) ty))))))
