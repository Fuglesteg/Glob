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

(defentity circles-renderer
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
  (:start circles-renderer
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
              append (with-vec (r g b) (circle-color (entity-find-behavior circle 'circle-behavior))
                       (list r g b)))
        :buffer-type :shader-storage-buffer
        :usage-type :dynamic-draw)
       (%gl:bind-buffer-base :shader-storage-buffer 1 buffer)
       (setf color-buffer buffer))
     (slither/render/vertex::with-buffer buffer :shader-storage-buffer
       (slither/render/vertex:send-buffer-data
        (loop for circle in circles
              collect (vx (transform-size circle)))
        :buffer-type :shader-storage-buffer
        :usage-type :dynamic-draw)
       (%gl:bind-buffer-base :shader-storage-buffer 2 buffer)
       (setf radius-buffer buffer))))
  (:tick circles-renderer
   (let ((shader-program circles-shader-program))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'camera))
                                 (slither::transform-position (circles-renderer-camera circles-renderer)))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'window-height))
                                 slither/window:*window-height*)
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'window-width))
                                 slither/window:*window-width*)
     (let ((camera-behavior (entity-find-behavior (circles-renderer-camera circles-renderer) 'slither::camera)))
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

(defmethod circles-renderer-update-color ((circles-renderer circles-renderer)
                                          (circle slither::entity))
  (let ((color (circle-color (entity-find-behavior circle 'circle-behavior))))
    (slither/render/vertex::with-bound-buffer (circles-renderer-color-buffer circles-renderer) :shader-storage-buffer
      (let ((color-buffer (%gl:map-buffer :shader-storage-buffer :write-only))
            (position (* (position circle (circles-renderer-circles circles-renderer)) 3)))
        (unwind-protect
             (progn
               (setf (cffi:mem-aref color-buffer :float position) (coerce (vx color) 'single-float)
                     (cffi:mem-aref color-buffer :float (+ position 1)) (coerce (vy color) 'single-float)
                     (cffi:mem-aref color-buffer :float (+ position 2)) (coerce (vz color) 'single-float)))
          (%gl:unmap-buffer :shader-storage-buffer))))))

(defmethod circles-renderer-update-radius ((circles-renderer circles-renderer)
                                           (circle slither::entity))
  (let ((radius (vx (transform-size circle))))
    (slither/render/vertex::with-bound-buffer (circles-renderer-radius-buffer circles-renderer) :shader-storage-buffer
      (let ((color-buffer (%gl:map-buffer :shader-storage-buffer :write-only))
            (position (position circle (circles-renderer-circles circles-renderer))))
        (unwind-protect
             (progn
               (setf (cffi:mem-aref color-buffer :float position) (coerce radius 'single-float)))
          (%gl:unmap-buffer :shader-storage-buffer))))))

(defbehavior inertia-behavior
    ((velocity
      :initarg :velocity
      :initform (vec2 0.0 0.0)
      :accessor inertia-velocity))
  (:tick (inertia entity)
   (with-vec (x y) 
       (nv+ (transform-position entity) (v* (inertia-velocity inertia) *dt*))))
  (inertia-velocity+ (inertia entity vector-to-add)
                     (with-accessors ((velocity inertia-velocity)) inertia
                       (setf velocity
                             (v+ velocity vector-to-add)))))

(defbehavior circle-behavior
  ((color
    :initarg :color
    :initform (random-color)
    :accessor circle-color)))

(defbehavior circle-move-behavior
    ()
  (:start (circle-behavior entity)
   (with-slots (position rotation) entity
     (with-slots (velocity) (entity-find-behavior entity 'inertia-behavior)
       (setf position (random-position-on-edge)
             rotation (random-float 360)
             velocity (nv+ (rotation->vec2 rotation) (+ 0.8 (random-float 2)))))))
  (:tick (circle-behavior entity)
   (when (out-of-bounds-p entity)
     (with-slots (position rotation) entity
       (with-slots (velocity) (entity-find-behavior entity 'inertia-behavior)
         (setf position (random-position-on-edge)
               rotation (random-float 360)
               velocity (nv* (rotation->vec2 rotation) (+ 0.8 (random-float 2)))))))))

(defentity circle
    ()
  (:behaviors
   (make-instance 'circle-behavior)
   (make-instance 'circle-collision)
   (make-instance 'circle-move-behavior)
   (make-instance 'inertia-behavior)
   (make-instance 'sticky-behavior))
  (:start circle
   (setf (transform-size circle)
         (let ((random-size (+ 0.4 (random-float))))
           (vec2 random-size random-size)))))

(defvar *player* nil)

(defentity player
    ((captured-circles
      :initform nil))
  (:behaviors
   (make-instance 'circle-behavior))
  (:start player
   (setf *player* player))
  (:tick player
   (setf (transform-position player)
         (v+ (transform-position player)
             (v* (v- (normalized-mouse-position) 0.5)
                 (vec2 1 -1) 10 *dt*)))))

(defmethod player-captured-circles ((player player))
  (slot-value player 'captured-circles))

(defmethod (setf player-captured-circles) (new-value
                                           (player player))
  (setf (slot-value player 'captured-circles) new-value)
  (setf (smooth-camera-zoom *camera*) (- 0.06 (* 0.001 (length new-value)))))
                      

(defvar *camera* nil)

(defclass interpolator ()
  ((start
    :initarg :start
    :initform 0
    :accessor interpolator-start
    :type number)
   (end
    :initarg :end
    :initform 1
    :accessor interpolator-end
    :type number)
   (value
    :initarg :value
    :initform 0
    :accessor interpolator-value
    :type number)))

(defmethod interpolator-increment ((interpolator interpolator)
                                   (value number))
  (incf (interpolator-value interpolator)
        value)
  (+ (interpolator-start interpolator)
     (* (- (interpolator-end interpolator)
           (interpolator-start interpolator))
        (smoothstep (interpolator-value interpolator)
                    0
                    1))))

(defmethod interpolator-reset ((interpolator interpolator)
                               (start number)
                               (end number)
                               &optional
                               (value 0))
  (setf (interpolator-start interpolator) start
        (interpolator-end interpolator) end
        (interpolator-value interpolator) value))
  

(defentity smooth-camera
    ((target
      :initarg :target
      :initform nil
      :accessor smooth-camera-target)
     (zoom-interpolator
      :initarg :zoom-interpolator
      :initform (make-instance 'interpolator
                               :end 0.06)
      :accessor smooth-camera-zoom-interpolator))
  (:behaviors
   (make-instance 'slither::camera :zoom 0.06))
  (:start entity
   (setf *camera* entity))
  (:tick entity
   (with-accessors ((target smooth-camera-target)) entity
     (nv+ (transform-position entity)
          (v* (v- (transform-position target)
                  (transform-position entity))
              6
              *dt*))
     (setf (slither::camera-zoom (entity-find-behavior entity 'slither::camera))
           (interpolator-increment
            (smooth-camera-zoom-interpolator entity)
            *dt*)))))

(defmethod (setf smooth-camera-zoom) ((value number)
                                      (smooth-camera smooth-camera))
  (interpolator-reset (smooth-camera-zoom-interpolator smooth-camera)
                      (slither::camera-zoom (entity-find-behavior
                                             smooth-camera
                                             'slither::camera))
                      value))

(defbehavior sticky-behavior
    ((offset
      :initarg :offset
      :initform (vec2 0 0)
      :accessor sticky-offset)
     (target
      :initarg :target
      :initform nil
      :accessor sticky-target))
  (:tick (sticky-behavior entity)
   (with-accessors ((target sticky-target)
                    (offset sticky-offset)) sticky-behavior
     (when target
       (setf (transform-position entity)
             (v+ (transform-position target) 
                 offset))))))

(defmethod circles-colliding-p ((circle-1 slither::entity) (circle-2 slither::entity))
  (with-accessors ((circle-1-position transform-position)
                   (circle-1-size transform-size)) circle-1
    (with-accessors ((circle-2-position transform-position)
                     (circle-2-size transform-size)) circle-2
      (let ((circle-1-radius (vx circle-1-size))
            (circle-2-radius (vx circle-2-size)))
        (< (abs (vdistance circle-2-position circle-1-position))
           (+ circle-1-radius circle-2-radius))))))

(defmethod circle-colliding-player-p ((circle circle))
  (or (circles-colliding-p circle *player*)
      (find-if (lambda (captured-circle)
                 (circles-colliding-p captured-circle circle))
               (player-captured-circles *player*))))

(defbehavior circle-collision
    ((enabled
      :initarg :enabled
      :initform t
      :accessor circle-collision-enabled))
  (:tick (circle-collision entity)
   (when (and (circle-collision-enabled circle-collision)
              (circle-colliding-player-p entity))
     (let* ((inertia-behavior (entity-find-behavior entity 'inertia-behavior))
            (velocity (inertia-velocity inertia-behavior)))
       (if (< 0.1 (vdistance velocity
                             (v* (v- (normalized-mouse-position) 0.5) ; FIXME: Wtf - change to player velocity
                                 (vec2 1 -1) 10 *dt*)))
           (inertia-velocity+ inertia-behavior entity
                              (v+ (v* (v- velocity) 8 *dt*)
                                  (v* (v- (normalized-mouse-position) 0.5) ; FIXME: Wtf - change to player velocity
                                      (vec2 1 -1) 40 *dt*)))
           (with-accessors ((target sticky-target)
                            (offset sticky-offset)) (entity-find-behavior entity 'sticky-behavior)
             (push entity (player-captured-circles *player*))
             (setf velocity 0
                   target *player*
                   offset (v- (transform-position entity)
                              (transform-position *player*))
                   (circle-collision-enabled circle-collision) nil)))))))

(defparameter *spawn-bounds-padding* 0.1)

(defmethod out-of-bounds-p ((circle circle))
  (let ((camera-zoom (slither::camera-zoom
                      (entity-find-behavior *camera* 'slither::camera))))
  (with-vec (player-x player-y) (transform-position *player*) 
    (with-vec (x y) (transform-position circle)
      (or (< x (- player-x (/ 1 camera-zoom) *spawn-bounds-padding*))
          (> x (+ player-x (/ 1 camera-zoom) *spawn-bounds-padding*))
          (< y (- player-y (/ 1 camera-zoom) *spawn-bounds-padding*))
          (> y (+ player-y (/ 1 camera-zoom) *spawn-bounds-padding*)))))))

(defmacro random-case (&body cases)
  "Evaluate a random case of `cases`.
Case is determined at runtime."
  `(case (random ,(length cases))
     ,@(loop for case in cases
             for i from 0
             collect `(,i ,case))))

(defun random-position-on-edge ()
  (with-vec (player-x player-y) (transform-position *player*) 
    (let* ((camera-zoom (coerce (slither::camera-zoom
                                 (entity-find-behavior *camera* 'slither::camera))
                                'single-float))
           (min-x (- player-x (/ 1 camera-zoom) *spawn-bounds-padding*))
           (max-x (+ player-x (/ 1 camera-zoom) *spawn-bounds-padding*))
           (min-y (- player-y (/ 1 camera-zoom) *spawn-bounds-padding*))
           (max-y (+ player-y (/ 1 camera-zoom) *spawn-bounds-padding*)))
      (random-case
        (vec2 (random-case min-x max-x)
              (random-float max-y))
        (vec2 (random-float max-y)
              (random-case min-y max-y))))))

(define-texture logo #P"./logo.png")

(defbehavior texture-renderer
    ()
  (:tick (texture-renderer entity)
   (with-accessors ((position transform-position)
                    (size transform-size)) entity
     (slither/render::draw-texture position size logo))))

#+nil(add-entity (make-instance 'slither::entity :behaviors (list (make-instance 'slither::camera)
                                                             (make-instance 'texture-renderer))))

(define-array-texture simple-font-array-texture #P"./font.png"
  :width 6
  :height 8)

(defbehavior simple-font-renderer
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
                                (#\} 93) (#\Newline 100)))))
     (let ((text (string->font-char-codes (font-renderer-text font-renderer))))
       (loop for char in text
             for x from 0
             with y = 0
             when (= char 100)
             do (decf y 2)
                (setf x 0)
             else
             do (slither/render::draw-array-texture (v+ position (vec2 (* 1.2 x) y))
                                                    (vec2 0.6 0.8)
                                                    char
                                                    simple-font-array-texture)))))))

  

(defentity background
    ()
  (:tick background
   (draw-static)))

(defentity fps-counter
    ()
  (:behaviors (make-instance 'simple-font-renderer :text "0"))
  (:tick fps-counter
   (setf (font-renderer-text (entity-find-behavior fps-counter 'simple-font-renderer))
         (format nil "~$" (fps)))))


#+nil(add-entity (make-instance 'slither::entity
                                :size (vec2 1.0 1.0)
                                :behaviors (list (make-instance 'slither::camera)
                                                 (make-instance 'simple-font-renderer))))

#+nil(start-game 
      (lambda ()
        (let* ((player (make-instance 'player
                                      :size (vec2 1.0 1.0)))
               (circles (append 
                         (list player) 
                         (loop repeat 40
                               collect (make-instance 'circle))))
               (camera (make-instance 'smooth-camera :target player)))
          (add-entity camera)
          (apply #'add-entity circles)
          (add-entity (make-instance 'circles-renderer
                                     :circles circles
                                     :camera camera))
          (add-entity (make-instance 'fps-counter)))))