;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry
;; find the score for a play from the game matrix
(define extract-entry 
  (lambda (game *game-association-list*)
    (cond 
        ((null? *game-association-list*) "invalid play")
        ((equal? game (caar *game-association-list*)) (car *game-association-list*))
        (else (extract-entry game (cdr *game-association-list*))))))

;; test
;(extract-entry (make-play "c" "c") *game-association-list*) => (("c" "c") (3 3)) ;valid play
;(extract-entry (make-play "c" "d") *game-association-list*) => (("c" "d") (0 5)) ;valid play
;(extract-entry (make-play "d" "c") *game-association-list*) => (("d" "c") (5 0)) ;valid play
;(extract-entry (make-play "d" "d") *game-association-list*) => (("d" "d") (1 1)) ;valid play
;(extract-entry (make-play "r" "d") *game-association-list*) => invalid play      ;invalid play

;; make matrix of scores of games between every possible pair of strategies in a given strategy list
(define strats-performance-comparison 
  (lambda (strats-list)
    (if (null? (cdr strats-list))
        (strat-performance (car strats-list) strats-list)
        (cons (strat-performance (car strats-list) strats-list)
              (strats-performance-comparison (cdr strats-list))))))
;; test
;(define strats-list (list NASTY PATSY SPASTIC EGALITARIAN EYE-FOR-EYE))
;(strats-performance-comparison strats-list)

;; make matrix of scores of every game between a strategy and 
;; strategies in a given strategy list
(define strat-performance 
  (lambda (strat strats-list)
    ;redefine playloop localy to return game results instead of printing them
    (define (play-loop strat0 strat1)
      (define (play-loop-iter strat0 strat1 count history0 history1 limit)
        (cond ((= count limit) (get-results history0 history1 limit))
	            (else (let ((result0 (strat0 history0 history1))
                          (result1 (strat1 history1 history0)))
                         (play-loop-iter strat0 strat1 (+ count 1)
                                        (extend-history result0 history0)
                                        (extend-history result1 history1)
                                        limit)))))
      (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
          (+ 90 (random 21))))

    ;redefinition of print-out-results to return the results instead of printing them
    (define (get-results history0 history1 number-of-games)
      (let ((scores (get-scores history0 history1)))
           (cons (* 1.0 (/ (car scores) number-of-games)) 
                 (* 1.0 (/ (cadr scores) number-of-games)))))     

    (if (null? (cdr strats-list))
        (cons (cons strat (car strats-list))
              (play-loop strat (car strats-list)))
        (cons (cons (cons strat (car strats-list))
                    (play-loop strat (car strats-list)))
              (strat-performance strat (cdr strats-list))))))

;; test
;(define strats-list (list nasty PATSY SPASTIC EGALITARIAN EYE-FOR-EYE))
;(strat-performance (car strats-list) strats-list)


(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))

  (let ((ds (count-instances-of "d" other-history))
	      (cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))


;; EYE-FOR-EYE but now looking recent two moves of the other player
(define EYE-FOR-TWO-EYES 
  (lambda (my-history other-history)
    (cond ((or (string=? (EYE-FOR-EYE my-history other-history) "c")
               (string=? (EYE-FOR-EYE my-history (rest-of-plays other-history)) "c")) "c")
          (else "d"))))

;; test
;(EYE-FOR-TWO-EYES '() (list "d" "d")) => "c"   ;my-history is empty
;(EYE-FOR-TWO-EYES (list "c" "c") (list "d" "c")) => "c" ;other-history has one "c" in recent two plays
;(EYE-FOR-n-EYES (list "c" "c") (list "d" "d") 2) => "d" ;other-history has no "c" in recent two plays

;; EYE-FOR-EYE but now looking recent n moves of the other player where n>=1
(define EYE-FOR-N-EYES 
  (lambda (my-history other-history n)
    (cond ((<= n 1) (EYE-FOR-EYE my-history other-history))
          ;if other-history has number of items less than n, call the apropriate EYE-FOR-N-EYES procedure.
          ((< (length other-history) n) (EYE-FOR-N-EYES my-history other-history (length other-history)))

          ((or (string=? (EYE-FOR-EYE my-history other-history) "c")
               (string=? (EYE-FOR-N-EYES my-history (rest-of-plays other-history) (- n 1)) "c")) "c")
          (else "d"))))

;; test
;(EYE-FOR-N-EYES '() (list "d" "d") 2) => "c"   ;my-history is empty
;(EYE-FOR-N-EYES (list "c" "c") (list "d" "c") 2) => "c" ;other-history has one "c" in recent two plays
;(EYE-FOR-N-EYES (list "c" "c") (list "d" "d") 2) => "d" ;other-history has no "c" in recent two plays
;(EYE-FOR-N-EYES '() (list "d" "d" "d" "d") 4) => "c"              ;my-history is empty
;(EYE-FOR-N-EYES (list "c" "c") (list "d" "c" "d" "d") 4) >= "c"   ;other-history has one "c" in recent <=N plays
;(EYE-FOR-N-EYES (list "c" "c") (list "d" "c") 4) >= "c"           ;test for history < N

;; plays strat0 for the first freq0 rounds in the
;; iterated game, then switches to strat1 for the next freq1 rounds, and so on.
;; round = (length my-history)
(define make-rotating-strategy
  (lambda (my-history other-history strat0 strat1 freq0 freq1)
    (cond ((= freq0 0) (strat1 my-history other-history))
          ((= freq1 0) (strat0 my-history other-history))
          ((= 0 (length my-history)) (strat0 my-history other-history))
          (else (let ((rem (remainder (length my-history)
                                      (+ freq0 freq1))))
                      (if (and (> rem 0)
                                (< rem freq0))
                          (strat0 my-history other-history)
                          (strat1 my-history other-history)))))))

;; test
;(make-rotating-strategy '() (list "c" "d") nasty patsy 3 2) => "d" ;0 initial history

;@ freq0 = 0
;(make-rotating-strategy '() (list "c" "d" "d") nasty patsy 0 2) => "c"

;@ freq1 = 0
;(make-rotating-strategy (list "d" "c" "d" "d") (list "c" "d" "d") nasty patsy 3 0) => "d"

;@ (length my-history)=freq0
;(make-rotating-strategy (list "d" "c" "d") (list "c" "d" "d") nasty patsy 3 2) => "c"

;@ (length my-history) >freq0 and (length my-history) <= (freq0 + freq1)
;(make-rotating-strategy (list "d" "c" "d" "d") (list "c" "d" "d") nasty patsy 3 2) => "c" 

;@ (length my-history) = (freq0 + freq1)
;(make-rotating-strategy (list "d" "c" "d" "d" "c") (list "c" "d" "d") nasty patsy 3 2) => "c" 

;;returns a new strategy that loops through a list of strategies passed as input, using the next
;;one in the list for each play, and then starting again at the beginning of the list when it has
;;used all the strategies
(define make-higher-order-spastic
  (lambda (my-history other-history strats-list)
    (let ((index (remainder (length my-history) (length strats-list))))
         (list-ref strats-list index))))

;; test
;@ (length my-history) = 0
;(make-higher-order-spastic '() (list "c" "d" "d") (list NASTY PATSY SPASTIC EYE-FOR-EYE)) >= NASTY

;@ (length my-history) = (length strats-list)
;(make-higher-order-spastic (list "c" "c" "d" "c") (list "c" "d" "d") (list NASTY PATSY SPASTIC EYE-FOR-EYE)) >= NASTY

;@ (length my-history) > (length strats-list)
;(make-higher-order-spastic (list "c" "c" "d" "d" "c" "d") (list "c" "d" "d" "c" "d" "d") (list NASTY PATSY SPASTIC EYE-FOR-EYE)) >= SPASTIC




;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

;(define *game-association-list*
;  (list (list (list "c" "c" "c") (list 4 4 4))
;        (list (list "c" "c" "d") (list 2 2 5))
;        (list (list "c" "d" "c") (list 2 5 2))
;        (list (list "d" "c" "c") (list 5 2 2))
;        (list (list "c" "d" "d") (list 0 3 3))
;        (list (list "d" "c" "d") (list 3 0 3))
;        (list (list "d" "d" "c") (list 3 3 0))
;        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))

