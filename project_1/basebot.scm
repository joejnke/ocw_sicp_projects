;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ u
      (* v t)
      (* 0.5
        a
        (square t)))))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0)
; (position 0 0 20 0)
; (position 0 5 10 10)
; (position 2 2 2 2)
; (position 5 5 5 5)


;; Problem 2
;; discriminant of a quadratic function ax^2 + bx + c => (b^2 - 4ac)
(define (discriminant a b c)
  (- (square b)   
      (* 4 a c)))

(define root1
  (lambda (a b c)
    (let ((discr (discriminant a b c)))
          (if (< discr 0) #f     ;; check if there is no real roots and return false if true
            (/ (+ (- b)
                  (sqrt discr))
                (* 2 a))))))

;(root1 1 (- 2) 1) => 1 ;;discriminant=0
;(root1 1 0 (- 1)) => 1 ;;discriminant>0
;(root1 2 3 4) => #f ;;discriminant<0

(define root2
  (lambda (a b c)
    (let ((discr (discriminant a b c)))
          (if (< discr 0) #f     ;; check if there is no real roots and return false if true
            (/ (- (- b)
                  (sqrt discr))
                (* 2 a))))))

;; complete these procedures and show some test cases
;(root2 1 (- 2) 1) => 1 ;;discriminant=0
;(root2 1 0 (- 1)) => -1 ;;discriminant>0
;(root2 2 3 4) => #f ;;discriminant<0

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (let ((r1 (root1 (* 0.5 (- gravity)) vertical-velocity elevation))
          (r2 (root2 (* 0.5 (- gravity)) vertical-velocity elevation)))
          (if r1                  ; check if there is real root
              (if (> r1 r2)       ; check and return the larger root
                  r1
                  r2)
              r1))))
; test 
; (time-to-impact 10 (- 10)) => #f ;time to get up to ground from height below ground
                                   ; with some initial velocity that can't make it up to ground
; (time-to-impact 0 0) => 0 ;time to get to ground from ground with zero initial velocity
; (time-to-impact 10 0) => 2.0408 ;time to get to ground from ground => -b/a
; (time-to-impact 0 10) => 1.4285 ;time to get to ground from a given height => square-root of (-c/a)

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))
; test 
; (time-to-height 0 0 10) => #f ;time to get to height h from ground with zero initial velocity
; (time-to-height 10 10 10) => 2.0408 ;time to get to height h from height h => -b/a
; (time-to-height 0 10 5) => 1.0101 ;time to get to half-way to ground from a given height

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (* (* velocity
          (cos (degree2radian angle)))
       (time-to-impact (* velocity (sin (degree2radian angle)))
                       elevation))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; (travel-distance-simple 1 45 0) => 20.3289m ;at an angle of 0 (straight horizontal)
;; (travel-distance-simple 1 45 90) => 5.4964-e4m ;at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; (travel-distance-simple 1 45 45) => 207.6278m ;at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

; compare the distance for start-angle and the distance for
; the angle with the max distance of the angle set [start-angle+1 end-angle]
(define angle-of-max-dist 
  (lambda (start-angle end-angle velocity elevation) 
    (if (= start-angle end-angle)
        start-angle
        (let ((max-angle (angle-of-max-dist (+ start-angle 1) 
                                            end-angle 
                                            velocity 
                                            elevation)))
             (if (> (travel-distance-simple elevation velocity start-angle) 
              (travel-distance-simple elevation velocity max-angle))
            start-angle
            max-angle)))))

(define find-best-angle
  (lambda (velocity elevation)
    (angle-of-max-dist 0 90 velocity elevation)))

;test 
;for any velocity and elevation. It must return 45 degrees
;(find-best-angle 305 1) => 45 ;

;; find best angle
;; try for other velocities
;; try for other heights

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    ;;compute the new value of x y u and v and then call integrate with this values
    (let ((x (+ x0 (* u0 dt)))
          (y (+ y0 (* v0 dt)))
          (u (+ u0 (* (- (/ 1 m))
                      beta
                      u0
                      (sqrt (+ (square u0) (square v0)))
                      dt)))
          (v (+ v0 (* dt
                      (- (+ g
                            (* (/ 1 m)
                                beta
                                v0
                                (sqrt (+ (square u0) (square v0))))))))))
          (if (<= y 0)       ;if the ball hit the ground
              (list x y (sqrt (+ (square u) (square v)))) ;return list of values of x, y and velocity magnitude
              (integrate x y u v dt g m beta))))) ;keep computing until it hits the ground
              

(define travel-distance
  (lambda (elevation velocity-mag angle)
    (let ((x0 0)
          (y0 elevation)
          (u0 (* velocity-mag (cos (degree2radian angle))))
          (v0 (* velocity-mag (sin (degree2radian angle)))))
         (car (integrate x0 y0 u0 v0 alpha-increment gravity mass beta)))))


;; RUN SOME TEST CASES
;(travel-distance 1 45 45) => 92.2306 ;need to be less than (travel-distance-simple 1 45 45)
;(travel-distance 1 40 45) => 81.6678 ;need to be less than (travel-distance-simple 1 40 45)
;(travel-distance 1 35 45) => 70.3003 ;need to be less than (travel-distance-simple 1 35 45)

;; what about Denver?

;; Problem 7
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

; construct set of valid angles to reach target with in any angle range. e.g: the range [-90 90] degrees => (cons (- 90) 90)
(define collect-reach-angles
  (lambda (velocity-mag elevation distance angle-range)
    (define reacheable?
      (lambda (angle)
        (if (and (time-to-impact (* velocity-mag (sin (degree2radian angle))) 
                            elevation) ;if hits ground
                 (< (abs (- distance (travel-distance elevation
                                                      velocity-mag
                                                      angle)))
                    0.01)) ;if travel distance is close to target diatance
            #t
            #f)))

    (define first-reacheable-angle 
      (lambda (angle-range)
        (if (>= (car angle-range) (cdr angle-range)) ;if there is only one angle inthe angle-range
            (if (reacheable? (car angle-range))
                (car angle-range)
                `())
            (if (reacheable? (car angle-range))  ;check if first angle in the range is valid
                (car angle-range)
                (first-reacheable-angle (cons (+ 1 (car angle-range))
                                              (cdr angle-range)))))))

    (let ((fra (first-reacheable-angle angle-range)))
         (if (null? fra)
              fra
              (cons fra (first-reacheable-angle (cons (+ 1 fra) (cdr angle-range))))))))

;return time taken to reach a target
(define integrate-time
  (lambda (x0 y0 u0 v0 dt distance time g m beta)
    (if (and (<= y0 0) (< (abs (- x0 distance)) 0.01))  ;if the ball hit the ground
        time        ;return the value of time at when the ball hits the ground

        ;;compute the new value of x y u v and time and then call integrate with this values
        (let ((temp_time (+ time dt))
              (x (+ x0 (* u0 dt)))
              (y (+ y0 (* v0 dt)))
              (u (+ u0 (* (- (/ 1 m))
                          beta
                          u0
                          (sqrt (+ (square u0) (square v0)))
                          dt)))
              (v (+ v0 (* dt
                          (- (+ g
                                (* (/ 1 m)
                                   beta
                                   v0
                                   (sqrt (+ (square u0) (square v0))))))))))
              (integrate-time x y u v dt distance temp_time g m beta)))))

(define travel-time
  (lambda (elevation velocity-mag distance angle)
    (let ((x0 0)
          (y0 elevation)
          (u0 (* velocity-mag (cos (degree2radian angle))))
          (v0 (* velocity-mag (sin (degree2radian angle)))))
         (integrate-time x0 y0 u0 v0 alpha-increment distance 0 gravity mass beta))))

; return the angle to achive the minimum travel time from set of valid angles
(define optimal-angle
  (lambda (elevation velocity-mag distance angles-set)
    (if (or (not (pair? angles-set)) (null? angles-set))
        angles-set
        (let ((next-angle (optimal-angle elevation velocity-mag distance (cdr angles-set))))
                                                                             
          (if (or (null? next-angle) 
                  (< (travel-time elevation velocity-mag distance (car angles-set))
                     (travel-time elevation velocity-mag distance next-angle)))
              (car angles-set)
              next-angle)))))

;calculate the best angle to throw a ball
(define angle-to-throw
  (lambda (velocity-mag elevation distance mass beta gravity)
    (let ((angles-set (collect-reach-angles velocity-mag 
                                             elevation
                                             distance
                                             (cons (- 90) 90))))
         (if (null? angles-set)
              `()
             (optimal-angle elevation velocity-mag distance angles-set)))))

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

;test 
;(angle-to-throw 45 1 0 mass beta gravity) => -90 ;throwing down to ground to 0 lateral distance from a given height

;; Problem 8
(define (bounce-travel-distance elevation velocity-mag angle num-bounce)
  (if (= 0 num-bounce)
      (travel-distance elevation velocity-mag angle)
      (+ (travel-distance elevation velocity-mag angle) 
         (bounce-travel-distance 0 (/ velocity-mag 2.0) angle (- num-bounce 1)))))
;; test
;(bounce-travel-distance 1 45 45 0) 
;Value: 92.2306 ;need to be same as (travel-distance 1 45 45)

;(bounce-travel-distance 1 45 45 1)
;Value: 130.6107 ;need to be greater than (bounce-travel-distance 1 45 45 0) 
                 ;by (travel-distance-simple 0 (/ 45 2.0) 45)
;(bounce-travel-distance 1 35 45 10) 
;Value: 106.2171

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate the maximum achievable distance, distance where the ball's speed will be 0,
;; and number of bounces to reach there.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (max-travel-distance elevation velocity-mag angle)
  (define (find-num-bounce init-num-bounce)
    (if (= (bounce-travel-distance elevation velocity-mag angle init-num-bounce)
            (bounce-travel-distance elevation velocity-mag angle (+ 1 init-num-bounce)))
          (list init-num-bounce (bounce-travel-distance elevation velocity-mag angle init-num-bounce))
          (find-num-bounce (+ 1 init-num-bounce))))
  (find-num-bounce 0))
;; test
;(max-travel-distance 1 45 45 10)  
;Value: 44          

;; Problem 9
