(uiop:define-package glob
  (:use #:cl 
        #:slither 
        #:slither/utils
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices)
(:import-from #:slither/input :key-held-p))

(in-package :glob)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro random-case (&body cases)
    "Evaluate a random case of `cases`.
Case is determined at runtime."
    `(case (random ,(length cases))
       ,@(loop for case in cases
               for i from 0
               collect `(,i ,case)))))

(define-fragment-shader circles-fragment-shader :path #P"./circles.frag")
(define-shader-program circles-shader-program
  :vertex-shader slither/render::static-vertex-shader
  :fragment-shader circles-fragment-shader
  :uniforms '(camera zoom window-height window-width))

(defvar *circles* nil)

(defentity circles-renderer
    ((position-buffer
      :initform nil
      :accessor circles-renderer-position-buffer)
     (color-buffer
      :initform nil
      :accessor circles-renderer-color-buffer)
     (radius-buffer
      :initform nil
      :accessor circles-renderer-radius-buffer))
  (:start circles-renderer
   (with-slots (position-buffer
                color-buffer
                radius-buffer) circles-renderer
     (slither/render/vertex::with-buffer buffer :shader-storage-buffer
       (slither/render/vertex:send-buffer-data
        (loop for circle in *circles*
              append (with-vec (x y) (slither::transform-position circle)
                       (list x y)))
        :buffer-type :shader-storage-buffer
        :usage-type :stream-draw)
       (%gl:bind-buffer-base :shader-storage-buffer 0 buffer)
       (setf position-buffer buffer))
     (slither/render/vertex::with-buffer buffer :shader-storage-buffer
       (slither/render/vertex:send-buffer-data
        (loop for circle in *circles*
              append (with-vec (r g b) (circle-color (entity-find-behavior circle 'circle-behavior))
                       (list r g b)))
        :buffer-type :shader-storage-buffer
        :usage-type :dynamic-draw)
       (%gl:bind-buffer-base :shader-storage-buffer 1 buffer)
       (setf color-buffer buffer))
     (slither/render/vertex::with-buffer buffer :shader-storage-buffer
       (slither/render/vertex:send-buffer-data
        (loop for circle in *circles*
              collect (vx (transform-size circle)))
        :buffer-type :shader-storage-buffer
        :usage-type :dynamic-draw)
       (%gl:bind-buffer-base :shader-storage-buffer 2 buffer)
       (setf radius-buffer buffer))))
  (:tick circles-renderer
   (let ((shader-program circles-shader-program))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'camera))
                                 (slither::transform-position *camera*))
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'window-height))
                                 slither/window:*window-height*)
     (slither::set-uniform-value shader-program
                                 (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'window-width))
                                 slither/window:*window-width*)
     (let ((camera-behavior (entity-find-behavior *camera* 'slither::camera)))
       (when camera-behavior
         (slither::set-uniform-value shader-program
                                     (slither::uniform-location (slither/render/shader-program::get-uniform shader-program 'zoom))
                                     (slither::camera-zoom camera-behavior))))
     (slither/render/vertex::with-bound-buffer (circles-renderer-position-buffer circles-renderer) :shader-storage-buffer
       (let ((buffer-data (%gl:map-buffer :shader-storage-buffer :write-only)))
         (unwind-protect
              (loop for circle in *circles*
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
            (position (* (position circle *circles*) 3)))
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
            (position (position circle *circles*)))
        (unwind-protect
             (progn
               (setf (cffi:mem-aref color-buffer :float position) (coerce radius 'single-float)))
          (%gl:unmap-buffer :shader-storage-buffer))))))

(defmethod out-of-bounds-p ((entity slither::entity))
  (let ((left (- (* *arena-width* 0.5)))
        (right (* *arena-width* 0.5))
        (top (* *arena-height* 0.5))
        (bottom (- (* *arena-height* 0.5)))
        (position (transform-position entity))
        (radius (* (circle-radius entity) 2)))
    (or (> (+ (vx position) radius) right)
        (< (- (vx position) radius) left)
        (> (+ (vy position) radius) top)
        (< (- (vy position) radius) bottom))))

(defbehavior inertia-behavior
    ((velocity
      :initarg :velocity
      :initform (vec2 0.0 0.0)
      :accessor inertia-velocity))
  (:tick (inertia entity)
   (with-vec (x y)
       (nv- (inertia-velocity inertia) (v* (inertia-velocity inertia) *dt*)) ; Friction
       (nv+ (transform-position entity) (v* (inertia-velocity inertia) *dt*)))
   (when (out-of-bounds-p entity)
     (inertia-velocity+ inertia (v* (vunit (transform-position entity)) -1 10 *dt*)))))

(defmethod inertia-velocity+ (inertia vector-to-add)
  (with-accessors ((velocity inertia-velocity)) inertia
    (setf velocity
          (v+ velocity vector-to-add))))

(defbehavior circle-behavior
  ((color
    :initarg :color
    :initform (random-color)
    :accessor circle-color)))

(defmethod circle-interest ((circle slither::entity))
  "Returns circle of interest, closest circle, and if it's a threat or a target"
  (let (interest (closest-distance 20000))
    (dolist (current-circle *circles*)
      (unless (eq current-circle circle)
        (let ((distance (- (transform-distance circle current-circle)
                           (+ (* (circle-radius circle) 2)
                              (* (circle-radius current-circle) 2)))))
          (when (< 5 distance)
            (when (< distance closest-distance)
              (setf interest current-circle
                    closest-distance distance))))))
    (values interest
            (if interest
                (cond
                  ((< *eat-forgiveness* (- (circle-radius circle)
                                           (circle-radius interest)))
                   :target)
                  ((< *eat-forgiveness* (- (circle-radius interest)
                                           (circle-radius circle)))
                   :threat))
                nil))))

(defbehavior circle-move-behavior
    ((think-delay
      :initform (random-float 2.0))
     (direction
      :initform (vec2 (random-float) (random-float))))
  (:start (circle-behavior entity)
   (circle-reset entity))
  (:tick (circle-move entity)
   (with-slots (think-delay direction) circle-move
     (inertia-velocity+ (entity-find-behavior entity 'inertia-behavior) (v* direction 2 *dt*))
     (decf think-delay *dt*)
     (when (<= think-delay 0)
       (setf think-delay (random-float 2.0))
       (multiple-value-bind (interest type) (circle-interest entity)
         (when interest
           (case type
             (:target (setf direction (vunit (v- (transform-position interest)
                                                 (transform-position entity)))))
             (:threat (setf direction (vunit (v- (transform-position entity)
                                                 (transform-position interest))))))))))))

(defmethod circle-reset ((circle slither::entity))
  (with-slots (position size) circle
    (with-slots (velocity) (entity-find-behavior circle 'inertia-behavior)
      (setf position (random-position)
            velocity (vec2 0 0)
            size (let ((random-size (+ 0.4 (random-float))))
                   (vec2 random-size random-size)))))
  (let ((circles-renderer (entities-find-entity 'circles-renderer)))
    (when circles-renderer
      (circles-renderer-update-color circles-renderer circle)
      (circles-renderer-update-radius circles-renderer circle))))

(defentity circle
    ()
  (:behaviors
   (make-instance 'circle-behavior)
   (make-instance 'circle-collision)
   (make-instance 'circle-move-behavior)
   (make-instance 'inertia-behavior))
  (:start circle
   (setf (transform-size circle)
         (let ((random-size (+ 0.4 (random-float))))
           (vec2 random-size random-size)))))

(defmethod circle-radius ((circle slither::entity))
  (vx (transform-size circle)))

(defmethod (setf circle-radius) (new-value (circle slither::entity))
  (let ((new-value (clamp new-value 0 2000)))
    (setf (transform-size circle)
          (vec2 new-value new-value)))
  (circles-renderer-update-radius (entities-find-entity 'circles-renderer)
                                  circle))

(defmethod circles-colliding-p ((circle-1 slither::entity) (circle-2 slither::entity))
  (with-accessors ((circle-1-position transform-position)
                   (circle-1-size transform-size)) circle-1
    (with-accessors ((circle-2-position transform-position)
                     (circle-2-size transform-size)) circle-2
      (let ((circle-1-radius (* (vx circle-1-size) 2))
            (circle-2-radius (* (vx circle-2-size) 2))
            (distance (abs (vdistance circle-2-position circle-1-position))))
        (values
         (< distance
            (- circle-1-radius circle-2-radius))
         distance)))))

(defparameter *eat-forgiveness* 0.2)

(defbehavior circle-collision
    ()
  (:tick (circle-collision entity)
   (loop for circle in *circles*
         do (when (and (circles-colliding-p entity circle)
                       (> (- (circle-radius entity)
                             (circle-radius circle))
                          *eat-forgiveness*))
                  (circle-eat entity circle)))))

(defmethod circle-eat ((eater slither::entity) (food slither::entity))
  (let ((food-to-eat (clamp (* 10 *dt*) 0 (circle-radius food))))
    (incf (circle-radius eater) food-to-eat)
    (decf (circle-radius food) food-to-eat))
  (when (>= 0.0 (circle-radius food))
    (if (eql *player* food)
        (player-lost)
        (circle-reset food)))
  (when (eql *player* eater)
    (setf *final-score* (score))
    (setf (smooth-camera-zoom *camera*) (clamp (- 0.06 (* 0.05 (smoothstep (/ (vx (transform-size eater)) 10) 0 1)))
                                               0.000001
                                               1))))

(defparameter *arena-width* 200)
(defparameter *arena-height* 200)

(defentity arena-borders
    ()
  (:tick entity
   (let ((left (- (* *arena-width* 0.5)))
         (right (* *arena-width* 0.5))
         (top (* *arena-height* 0.5))
         (bottom (- (* *arena-height* 0.5)))
         (color (vec4 1 1 1 0.8)))
     (draw-rectangle (vec2 0 top) (vec2 *arena-width* 1.0) color)
     (draw-rectangle (vec2 0 bottom) (vec2 *arena-width* 1.0) color)
     (draw-rectangle (vec2 left 0) (vec2 1.0 *arena-height*) color)
     (draw-rectangle (vec2 right 0) (vec2 1.0 *arena-height*) color))))

(defun random-position ()
  (vec2 (- (random-float *arena-width* 1000) (* *arena-width* 0.5))
        (- (random-float *arena-height* 1000) (* *arena-height* 0.5))))

(defvar *player* nil)

(defentity player
    ()
  (:behaviors
   (make-instance 'circle-behavior)
   (make-instance 'inertia-behavior)
   (make-instance 'circle-collision))
  (:start player
   (setf *player* player))
  (:tick player
   (unless *game-over-menu*
     (inertia-velocity+ (entity-find-behavior player 'inertia-behavior)
                        (v* (vclamp -0.5 (v* (normalized-screen-space-mouse-position) 5) 0.5)
                            (vec2 1 -1) 4 *dt*)))
   ;; Check if player has won or lost
   (let ((player-won-game (<= 2000 (circle-radius player)))
         (player-lost-game (find-if (lambda (circle) 
                                      (<= 2000 (circle-radius circle)))
                                    *circles*)))
     (cond
       (player-won-game (player-won))
       (player-lost-game (player-lost-game)))))

(defun player-won ()
  (unless *game-over-menu*
    (setf *game-over-menu*
          (list (make-instance 'game-won-header)
                (make-instance 'restart-button)))
    (apply #'append-entity *game-over-menu*)))

(defun player-lost-game ()
  (unless *game-over-menu*
    (setf *game-over-menu*
          (list (make-instance 'game-over-header)
                (make-instance 'restart-button)))
    (apply #'append-entity *game-over-menu*)))

(defvar *game-over-menu* nil)
  
(defun player-lost ()
  (unless *game-over-menu*
    (setf *game-over-menu*
          (list (make-instance 'game-over-header)
                (make-instance 'restart-button)
                #+nil(make-instance 'respawn-button)))
    (apply #'append-entity *game-over-menu*)))

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
  
(defvar *camera* nil)

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

(define-texture dot-texture #P"./dot.png")

(defentity texture-renderer
    ()
  (:behaviors (make-instance 'slither::camera))
  (:tick entity
   (with-accessors ((position transform-position)
                    (size transform-size)) entity
     (slither/render::draw-texture position size dot-texture :texture-scale (vec2 200 200)))))

(define-array-texture simple-font-array-texture #P"./font.png"
  :width 6
  :height 8)

(defun string->font-char-codes (string)
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
                  (#\} 93) (#\Newline 100))))

(defun draw-char-codes (char-codes position size)
  (loop for char in char-codes
        for x from 0
        with y = 0
        when (= char 100)
        do (decf y 2)
           (setf x 0)
        else
        do (slither/render::draw-array-texture (v+ (v- position (vec2 (* (vx size) 2 (length char-codes) 0.5) (* (vy size) 0.5)))
                                                   (vec2 (* (vx size) 2 x) y))
                                               (v* size (vec2 0.6 0.8))
                                               char
                                               simple-font-array-texture)))

(defun draw-text (text position size)
  (draw-char-codes (string->font-char-codes text)
                   position
                   size))

(defbehavior simple-font-renderer
    ((text
      :initarg :text
      :initform "text"
      :accessor font-renderer-text
      :type string))
  (:tick (font-renderer entity)
   (with-accessors ((position transform-position)
                    (size transform-size)) entity
     (draw-text (font-renderer-text font-renderer)
                position
                size))))

(defentity background
    ()
  (:tick background
     (draw-texture (vec2 0 0)
                   (vec2 (* *arena-width* 0.5) (* *arena-height* 0.5))
                   dot-texture
                   :texture-scale (vec2 100 100))))

(defentity fps-counter
    ()
  (:behaviors (make-instance 'simple-font-renderer :text "0"))
  (:tick fps-counter
   (setf (transform-position fps-counter) (slither/render:screen-space-position (vec2 -0.99 -0.98)))
   (setf (transform-size fps-counter) (slither/render:screen-space-scale (vec2 10 10)))
   (setf (font-renderer-text (entity-find-behavior fps-counter 'simple-font-renderer))
         (format nil "~$" (fps)))))

(defun score ()
  (floor (* (circle-radius *player*) 100)))

(defentity score-counter
    ()
  (:behaviors (make-instance 'simple-font-renderer :text "0"))
  (:tick score-counter
   (setf (transform-position score-counter) (slither/render:screen-space-position (vec2 -0.98 0.95)))
   (setf (transform-size score-counter) (slither/render:screen-space-scale (vec2 0.016 0.019)))
   (setf (font-renderer-text (entity-find-behavior score-counter 'simple-font-renderer))
         (format nil "Size: ~a" (score)))))

(defentity restart-button
    ()
  (:tick menu
   (let* ((position (vec2 0.0 0.0))
          (scale (vec2 0.12 0.03))
          (left (- (vx position) (* (vx scale) 0.5)))
          (right (+ (vx position) (* (vx scale) 0.5)))
          (bottom (- (vy position) (* (vy scale) 0.5)))
          (top (+ (vy position) (* (vy scale) 0.5)))
          (mouse-position (normalized-screen-space-mouse-position))
          (hover-p (and (< (vx mouse-position) right)
                        (> (vx mouse-position) left)
                        (< (vy mouse-position) top)
                        (> (vy mouse-position) bottom)))
          (color (if hover-p
                     (vec4 0.9 0.9 0.6 1.0)
                     (vec4 0.8 0.8 0.5 1.0))))
     (draw-rectangle (slither/render:screen-space-position position)
                     (slither/render:screen-space-scale scale)
                     color)
     (draw-text "Restart"
                (slither/render:screen-space-position (vec2 -0.0 0.0))
                (slither/render:screen-space-scale (vec2 0.016 0.019)))
     (when (and hover-p
                (key-held-p :left))
       (restart-game)))))

(defun respawn ()
  (apply #'remove-entity *game-over-menu*)
  (setf *game-over-menu* nil)
  (circle-reset *player*)
  (setf (circle-radius *player*) 1.0))

(defentity respawn-button
    ()
  (:tick menu
   (let* ((position (vec2 0.0 0.1))
          (scale (vec2 0.12 0.03))
          (left (- (vx position) (* (vx scale) 0.5)))
          (right (+ (vx position) (* (vx scale) 0.5)))
          (bottom (- (vy position) (* (vy scale) 0.5)))
          (top (+ (vy position) (* (vy scale) 0.5)))
          (mouse-position (normalized-screen-space-mouse-position))
          (hover-p (and (< (vx mouse-position) right)
                        (> (vx mouse-position) left)
                        (< (vy mouse-position) top)
                        (> (vy mouse-position) bottom)))
          (color (if hover-p
                     (vec4 0.9 0.9 0.6 1.0)
                     (vec4 0.8 0.8 0.5 1.0))))
     (draw-rectangle (slither/render:screen-space-position position)
                     (slither/render:screen-space-scale scale)
                     color)
     (draw-text "Respawn"
                (slither/render:screen-space-position (vec2 -0.0 0.1))
                (slither/render:screen-space-scale (vec2 0.016 0.019)))
     (when (and hover-p
                (key-held-p :left))
       (respawn)))))

(defvar *final-score* 0)

(defentity game-over-header
    ()
  (:tick header
   (draw-text (format nil "You lost")
              (slither/render:screen-space-position (vec2 0 0.8))
              (slither/render:screen-space-scale (vec2 0.026 0.029)))
   (draw-text (format nil "Your final score was: ~d" *final-score*)
              (slither/render:screen-space-position (vec2 0 0.7))
              (slither/render:screen-space-scale (vec2 0.026 0.029)))))

(defentity game-won-header
    ()
  (:tick header
   (draw-text (format nil "You Won!")
              (slither/render:screen-space-position (vec2 0 0.8))
              (slither/render:screen-space-scale (vec2 0.026 0.029)))
   (draw-text (format nil "Your final score was: ~d" *final-score*)
              (slither/render:screen-space-position (vec2 0 0.7))
              (slither/render:screen-space-scale (vec2 0.026 0.029)))))

(defun restart-game ()
  (setf slither::*entities* nil)
  (setf *game-over-menu* nil)
  (let* ((player (make-instance 'player
                                :size (vec2 1.0 1.0)))
         (circles (append 
                   (list player) 
                   (loop repeat 40
                         collect (make-instance 'circle))))
         (camera (make-instance 'smooth-camera :target player)))
    (add-entity camera)
    (add-entity (make-instance 'score-counter))
    (apply #'add-entity circles)
    (setf *circles* circles)
    (add-entity (make-instance 'circles-renderer))
    (add-entity (make-instance 'arena-borders))
    (add-entity (make-instance 'background))))

(defun start-glob ()
  (start-game
   #'restart-game))