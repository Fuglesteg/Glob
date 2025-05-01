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
  :uniforms '(circles camera zoom))

(defbehavior circles-renderer
    ((circles
      :initform nil
      :initarg :circles
      :accessor circles-renderer-circles)
     (circles-uniform-location
      :initform nil
      :initarg :circles-uniform-location
      :accessor circles-renderer-circles-uniform-location)
     (camera
      :initform nil
      :initarg :camera
      :accessor circles-renderer-camera))
  (:start (circles-renderer entity)
   (with-accessors ((circles-uniform-location circles-renderer-circles-uniform-location)) circles-renderer
     (setf circles-uniform-location 1 #+nil(slither/render/shader-program::uniform-location ;; TODO: WTHELLY
                                     (slither/render/shader-program:get-uniform
                                      (slither/render::shader-program *circles-renderable*)
                                      'circles)))))
  (:tick (circles-renderer entity)
   (let ((shader-program circles-shader-program))
     #+dev(when (key-held-p :r)
       (define-fragment-shader circles-fragment-shader :path "./circles.frag")
       (slither/render/shader-program:shader-program-recompile shader-program))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'camera))
                                 (slither::transform-position (circles-renderer-camera circles-renderer)))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'zoom))
                                 (slither::camera-zoom (find 'slither::camera (slither::entity-behaviors (circles-renderer-camera circles-renderer))
                                                             :key #'type-of)))
     (loop for circle in (circles-renderer-circles circles-renderer)
           for uniform-location from 1 by 3
           do (slither::set-uniform-value shader-program
                                 uniform-location
                                 (slither::transform-position circle))
              (slither::set-uniform-value shader-program
                                 (+ uniform-location 1)
                                 (circle-color circle))
              (slither::set-uniform-value shader-program
                                 (+ uniform-location 2)
                                 (circle-radius circle))))
   (slither/render::draw-static :shader-program circles-shader-program)))

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
     #+nil(setf position (vec2 0.7 0.4))
     (when (out-of-bounds-p circle)
       (setf position (random-location)
             rotation (random-float 360)
             velocity (+ 0.8 (random-float 2))))
     (setf position (v+ position 
                        (v* (rotation->vec2 rotation) velocity slither::*dt*))))))

(defmethod out-of-bounds-p ((circle circle))
  (with-slots (position) circle
    (with-vec (x y) position
      (or (< x 0)
          (> x 20)
          (< y 0)
          (> y 20)))))


#+nil(start-game 
 (lambda ()
   (let* ((camera (make-instance 'slither::entity :behaviors (list (make-instance 'slither::camera)
                                                                   (make-instance 'slither::move :speed 0.4)
                                                                   (make-instance 'slither::rectangle))))
         (circles (loop repeat 40
                        collect (make-instance 'circle))))
     (add-entity (make-instance 'slither::entity
                                :behaviors (list (make-instance 'circles-renderer
                                                                :circles circles
                                                                :camera camera))))
  (add-entity camera)
  (apply #'add-entity circles))))