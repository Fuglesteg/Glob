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

(define-fragment-shader circles-fragment-shader :path #P"./circles.frag")
(define-shader-program circles-shader-program
  :vertex-shader slither/render::static-vertex-shader
  :fragment-shader circles-fragment-shader
  :uniforms '(camera zoom window-height window-width))

(defparameter *bounding-width* 40)
(defparameter *bounding-height* 20)

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
   #+nil(setf *bounding-height* (or (and slither/window:*window-height* (* slither/window:*window-height* 0.01))
                               *bounding-height*)
         *bounding-width* (or (and slither/window:*window-width* (* slither/window:*window-width* 0.01))
                              *bounding-width*))
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
     (let ((camera-behavior (find 'slither::camera
                                  (slither::entity-behaviors (circles-renderer-camera circles-renderer))
                                  :key #'type-of)))
       (when camera-behavior
         (slither::set-uniform-value shader-program
                                     (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'zoom))
                                     (slither::camera-zoom camera-behavior))))
     (slither/render/vertex::with-bound-buffer (circles-renderer-position-buffer circles-renderer) :shader-storage-buffer
       (let ((buffer-data (%gl:map-buffer :shader-storage-buffer :write-only)))
         (unwind-protect
              (loop for circle in (circles-renderer-circles circles-renderer)
                    for i from 0 by 2
                    do (with-vec (x y) (slither::transform-position circle)
                         (setf (cffi:mem-aref buffer-data :float i) (coerce x 'single-float))
                         (setf (cffi:mem-aref buffer-data :float (1+ i)) (coerce y 'single-float))))
           (%gl:unmap-buffer :shader-storage-buffer))))
     (%gl:bind-buffer-base :shader-storage-buffer 0 (circles-renderer-position-buffer circles-renderer))
     (slither/render::draw-static :shader-program circles-shader-program))))

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

(defmethod out-of-bounds-p ((circle circle))
  (with-slots (position) circle
    (with-vec (x y) position
      (or (< x 0)
          (> x *bounding-width*)
          (< y 0)
          (> y *bounding-height*)))))

(defmacro random-case (&body cases)
  "Evaluate a random case of `cases`.
Case is determined at runtime."
  `(case (random ,(length cases))
     ,@(loop for case in cases
             for i from 0
             collect `(,i ,case))))

(defun random-position-on-edge ()
  (random-case
    (vec2 (random-case 0 *bounding-width*)
          (random (1+ *bounding-height*)))
    (vec2 (random (1+ *bounding-width*))
          (random-case 0 *bounding-height*))))

(slither/assets:defasset logo #P"./logo.png" :png)
(defvar *texture* (make-instance 'slither/render/texture:texture :asset 'logo))

(defbehavior texture-renderer
    ()
  (:tick (texture-renderer entity)
   (with-accessors ((position transform-position)
                    (size transform-size)) entity
     (slither/render::draw-texture position size *texture*))))

#+nil(add-entity (make-instance 'slither::entity :behaviors (list (make-instance 'slither::camera)
                                                             (make-instance 'texture-renderer))))

(slither/assets:defasset font #P"./font.png" :png)
(defvar *font-array-texture* (make-instance 'slither/render/array-texture:array-texture
                                            :asset 'font
                                            :width 6
                                            :height 8))

(defbehavior font-renderer
    ((text
      :initarg :text
      :initform "text"
      :accessor font-renderer-text
      :type string))
  (:tick (font-renderer entity)
   (with-accessors ((position transform-position)
                    (size transform-size)) entity
     (labels ((string->font-char-codes (string)
                (loop for char across string
                      collect (case char
                                (#\Space 0) (#\! 1) (#\" 2) (#\# 3) (#\$ 4)
                                (#\% 5) (#\& 6) (#\' 7) (#\( 8)
                                (#\) 9) (#\* 10) (#\+ 11) (#\, 12)
                                (#\- 13) (#\. 14) (#\/ 15) (#\0 16)
                                (#\1 17) (#\2 18) (#\3 19) (#\4 20)
                                (#\5 21) (#\6 22) (#\7 23) (#\8 24)
                                (#\9 25) (#\: 26) (#\; 27) (#\< 28)
                                (#\= 29) (#\> 30) (#\? 31) (#\@ 32)
                                (#\A 33) (#\B 34) (#\C 35) (#\D 36)
                                (#\E 37) (#\F 38) (#\G 39) (#\H 40)
                                (#\I 41) (#\J 42) (#\K 43) (#\L 44)
                                (#\M 45) (#\N 46) (#\O 47) (#\P 48)
                                (#\Q 49) (#\R 50) (#\S 51) (#\T 52)
                                (#\U 53) (#\V 54) (#\W 55) (#\X 56)
                                (#\Y 57) (#\Z 58) (#\[ 59) (#\\ 60)
                                (#\] 61) (#\^ 62) (#\_ 63) (#\` 64)
                                (#\a 65) (#\b 66) (#\c 67) (#\d 68)
                                (#\e 69) (#\f 70) (#\g 71) (#\h 72)
                                (#\i 73) (#\j 74) (#\k 75) (#\l 76)
                                (#\m 77) (#\n 78) (#\o 79) (#\p 80)
                                (#\q 81) (#\r 82) (#\s 83) (#\t 84)
                                (#\u 85) (#\v 86) (#\w 87) (#\x 88)
                                (#\y 89) (#\z 90) (#\{ 91) #+nil(#\: 92) ;; TODO: Wtf is 92?
                                (#\} 93)))))
     (let ((text (string->font-char-codes (font-renderer-text font-renderer))))
       (loop for char in text
             for i from 0
             do (slither/render::draw-array-texture (v+ position (vec2 (* (1+ 0.6) i) 0))
                                                    (vec2 0.6 0.8)
                                                    char
                                                    *font-array-texture*)))))))

#+nil(add-entity (make-instance 'slither::entity
                                :size (vec2 1.0 1.0)
                                :behaviors (list (make-instance 'slither::camera)
                                                 (make-instance 'font-renderer))))

#+nil(start-game 
 (lambda ()
   (let* ((camera (make-instance 'circle
                                 :size (vec2 1.0 1.0)
                                 :color (vec3 1.0 1.0 1.0)
                                 :radius 0.8
                                 :behaviors (list (make-instance 'slither::camera))))
         (circles (append 
                   (loop repeat 40
                        collect (make-instance 'circle))
                   (list camera))))
     (add-entity (make-instance 'slither::entity
                                :behaviors (list (make-instance 'circles-renderer
                                                                :circles circles
                                                                :camera camera))))
  (add-entity camera)
  (apply #'add-entity circles))))