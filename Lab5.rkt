#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 1000)
(define HEIGHT 800)

(define SHIP
  (above
   (triangle 25 "outline" "blue")
   (regular-polygon 15 8 "outline" "red")))

(define (add-star image)
  (place-image
   (circle 1 "solid" "white")
   (random WIDTH)
   (random HEIGHT)
   image))

(define RECT
  (rectangle WIDTH HEIGHT "solid" "black"))

(define (add-stars counter)
  (if (zero? counter)
      RECT
      (add-star (add-stars (sub1 counter)))))

(define-struct state (rotation x y move-state fuel) #:transparent) ; Define state as struct
(define initial-state (state 0 200 200 "stop" 1000))

(define-struct planet (x y color radius)) ; Define planet as struct

(define (bright-byte)
  (+ 50 (random 205)))

(define (random-color)
  (color (bright-byte) (bright-byte) (bright-byte)))

(define (random-planet)
  (make-planet (random WIDTH) (random HEIGHT) (random-color) (+ 10 (random 10))))

(define (generate-planets n)
  (if (= n 0)
      '()
      (cons (random-planet) (generate-planets (sub1 n)))))

(define planets (generate-planets 10))

(define (draw-planet planet image)
  (place-image
   (circle (planet-radius planet) "solid" (planet-color planet))
   (planet-x planet)
   (planet-y planet)
   image))

(define (draw-planets planets image)
  (if (empty? planets)
      image
      (draw-planet (first planets) (draw-planets (rest planets) image))))

(define BACKGROUND
  (draw-planets planets (add-stars 10)))

(define (should-finish n)
  #f)

(define (place-rocket state)
  (place-image
   (text (number->string (state-fuel state)) 30 "white")
   30 10
  (place-image
   (rotate (state-rotation state) SHIP) ; Image 
   (state-x state)
   (state-y state)
   BACKGROUND)))

(define (identity state) state)

(define STOP "stop")
(define MOVE "move")

(define (steer rotation key)
  (cond
    [(equal? key "right") (remainder (+ rotation 5) 360)]
    [(equal? key "left") (remainder (- rotation 5) 360)]
    [else rotation]))

(define (new-move-state state key)
  (cond
    [(<= (state-fuel state) 1) STOP]
    [(and (equal? key "up") (equal? (state-move-state state) STOP)) MOVE]
    [(and (equal? key "down") (equal? (state-move-state state) MOVE)) STOP]
    [else (state-move-state state)]))

(define (new-fuel-state state)
  (cond
    [(string=? (state-move-state state) MOVE) (- (state-fuel state) 1)]
    [(state-fuel state)]))

(define (key-action state key)
  (make-state
   (steer (state-rotation state) key)
   (state-x state)
   (state-y state)
   (new-move-state state key)
   (state-fuel state)))

(define (new-x-pos rotation x-pos move-state)
 (cond
  [(string=? move-state MOVE) (+ x-pos (sin (degrees->radians rotation)))]
  [else x-pos]))

(define (new-y-pos rotation y-pos move-state)
 (cond
  [(string=? move-state MOVE) (+ y-pos (cos (degrees->radians rotation)))]
  [else y-pos]))

(define (ship-tick state)
  (make-state
   (state-rotation state)
   (new-x-pos (state-rotation state) (state-x state) (state-move-state state))
   (new-y-pos (state-rotation state) (state-y state) (state-move-state state))
   (new-move-state state "")
   (if (colliding-with-planets? state planets)
       1000
       (new-fuel-state state))))

(define (colliding? x1 y1 x2 y2 r1 r2)
  (<= (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))) (+ r1 r2)))

(define (colliding-with-planet? state planet)
  (colliding? (state-x state) (state-y state) (planet-x planet) (planet-y planet)
              (image-width SHIP) (planet-radius planet)))

(define (colliding-with-planets? state planets)
  (cond
    [(empty? planets) #false]
    [(colliding-with-planet? state (first planets)) #true]
    [else (colliding-with-planets? state (rest planets))]))
  

(big-bang
 initial-state ; Initial state
 [on-key key-action]
 [on-tick ship-tick]
 [to-draw place-rocket]
 [stop-when should-finish])