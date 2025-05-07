(uiop:define-package glob
  (:use #:cl 
        #:slither 
        #:slither/utils
        #:org.shirakumo.fraf.math.vectors)
  (:import-from #:slither/input :key-held-p))

(in-package :glob)

(defentity player ()
  (:behaviors
   (make-instance 'slither::move)
   (make-instance 'slither::camera)
   (make-instance 'slither::rectangle)))

(define-fragment-shader circles-fragment-shader :path "./circles.frag")
(define-shader-program circles-shader-program
  :vertex-shader slither/render::static-vertex-shader
  :fragment-shader circles-fragment-shader
  :uniforms '(camera zoom window-height window-width))

(defbehavior circles-renderer
    ((circles
      :initform nil
      :initarg :circles
      :accessor circles-renderer-circles)
     (position-buffer
      :initform nil
      :accessor circles-renderer-position-buffer)
     (color-buffer
      :initform nil
      :accessor circles-renderer-color-buffer)
     (radius-buffer
      :initform nil
      :accessor circles-renderer-radius-buffer)
     (camera
      :initform nil
      :initarg :camera
      :accessor circles-renderer-camera))
  (:start (circles-renderer entity)
   (with-slots (position-buffer
                color-buffer
                radius-buffer
                circles) circles-renderer
     (slither/render/vertex::with-buffer buffer :shader-storage-buffer
       (slither/render/vertex:send-buffer-data
        (loop for circle in circles
              append (with-vec (x y) (slither::transform-position circle)
                       (list x y)))
        :buffer-type :shader-storage-buffer
        :usage-type :stream-draw)
       (%gl:bind-buffer-base :shader-storage-buffer 0 buffer)
       (setf position-buffer buffer))
     (slither/render/vertex::with-buffer buffer :shader-storage-buffer
       (slither/render/vertex:send-buffer-data
        (loop for circle in circles
              append (with-vec (r g b) (circle-color circle)
                       (list r g b)))
        :buffer-type :shader-storage-buffer)
       (%gl:bind-buffer-base :shader-storage-buffer 1 buffer)
       (setf color-buffer buffer))
     (slither/render/vertex::with-buffer buffer :shader-storage-buffer
       (slither/render/vertex:send-buffer-data
        (loop for circle in circles
              collect (circle-radius circle))
        :buffer-type :shader-storage-buffer)
       (%gl:bind-buffer-base :shader-storage-buffer 2 buffer)
       (setf radius-buffer buffer))))
     (:tick (circles-renderer entity)
   (let ((shader-program circles-shader-program))
     #+dev(when (key-held-p :r)
            (define-fragment-shader circles-fragment-shader :path "./circles.frag")
            (slither/render/shader-program:shader-program-recompile shader-program))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'camera))
                                 (slither::transform-position (circles-renderer-camera circles-renderer)))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'window-height))
                                 slither/window:*window-height*)
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'window-width))
                                 slither/window:*window-width*)
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'zoom))
                                 (slither::camera-zoom (find 'slither::camera
                                                             (slither::entity-behaviors (circles-renderer-camera circles-renderer))
                                                             :key #'type-of)))
       (slither/render/vertex::with-bound-buffer (circles-renderer-position-buffer circles-renderer) :shader-storage-buffer
         (let ((buffer-data (%gl:map-buffer :shader-storage-buffer :write-only)))
           #+nil(static-vectors:replace-foreign-memory buffer-data (static-vectors:static-vector-pointer positions) (length positions))
           (unwind-protect
                (loop for circle in (circles-renderer-circles circles-renderer)
                      for i from 0 by 2
                      do (with-vec (x y) (slither::transform-position circle)
                           (setf (cffi:mem-aref buffer-data :float i) (coerce x 'single-float))
                           (setf (cffi:mem-aref buffer-data :float (1+ i)) (coerce y 'single-float))))
             (%gl:unmap-buffer :shader-storage-buffer))))
     (%gl:bind-buffer-base :shader-storage-buffer 0 (circles-renderer-position-buffer circles-renderer))
     (slither/render::draw-static :shader-program circles-shader-program))))

(defun random-location ()
  (vec2 (random-float 20)
        (random-float 20)))

(defentity circle
  ((radius
    :initarg :radius
    :initform (+ 0.2 (random-float))
    :accessor circle-radius)
   (velocity
    :initarg :velocity
    :initform 1.0
    :accessor circle-velocity)
   (color
    :initarg :color
    :initform (random-color)
    :accessor circle-color))
  (:behaviors (make-instance 'slither::move))
  (:start circle
   (with-slots (position rotation slither/render/shader-program:shader-program) circle
     (setf rotation (random-float 360))))
  (:tick circle
   (with-slots (position rotation velocity) circle
     (when (out-of-bounds-p circle)
       (setf position (random-position-on-edge)
             rotation (random-float 360)
             velocity (+ 0.8 (random-float 2))))
     (setf position (v+ position 
                        (v* (rotation->vec2 rotation)
                            velocity
                            slither::*dt*))))))

(defparameter *bounding-distance* 40)

(defmethod out-of-bounds-p ((circle circle))
  (with-slots (position) circle
    (with-vec (x y) position
      (or (< x 0)
          (> x *bounding-distance*)
          (< y 0)
          (> y *bounding-distance*)))))

(defun random-position-on-edge ()
  (let ((random-edge (case (random 2) (0 0) (1 *bounding-distance*)))
        (random-point-on-axis (random (1+ *bounding-distance*))))
    (case (random 2)
      (0 (vec2 random-edge random-point-on-axis))
      (1 (vec2 random-point-on-axis random-edge)))))


#+nil(start-game 
 (lambda ()
   (let* ((camera (make-instance 'slither::entity :behaviors (list (make-instance 'slither::camera)
                                                                   (make-instance 'slither::move :speed 0.5)
                                                                   (make-instance 'slither::rectangle))))
         (circles (loop repeat 400
                        collect (make-instance 'circle))))
     (add-entity (make-instance 'slither::entity
                                :behaviors (list (make-instance 'circles-renderer
                                                                :circles circles
                                                                :camera camera))))
  (add-entity camera)
  (apply #'add-entity circles))))