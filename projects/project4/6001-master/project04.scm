;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  set up world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; MIT 6.001                                  Spring, 2007
;;; PROJECT 4

;;;========================================================================
;;; You can extend this part of the file to extend your world.
;;;========================================================================


;;; load up needed files
;;; load the random function from racket/base 
(#%require (only racket/base random))
(#%require (only racket/base error))
(load "objsys.scm")
(load "objtype.scm")

;;------------------------------------------------------------
;; Utils to connect places by way of exits

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

;;------------------------------------------------------------
;; Create our world...

;; a special place

(define heaven '())

(define (create-world)
  ; Create some places
  (let ((10-250 (create-place '10-250))
        (starbucks (create-place 'starbucks))
        (lobby-10 (create-place 'lobby-10))
        (grendels-den (create-place 'grendels-den))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (eecs-hq (create-place 'eecs-hq))
        (eecs-ug-office (create-place 'eecs-ug-office))
        (edgerton-hall (create-place 'edgerton-hall))
        (34-301 (create-place '34-301))
        (32-123 (create-place '32-123))
        (stata-center (create-place 'stata-center))
        (student-street (create-place 'student-street))
        (6001-lab (create-place '6001-lab))
        (building-13 (create-place 'building-13))
        (great-court (create-place 'great-court))
        (student-center (create-place 'student-center))
        (bexley (create-place 'bexley))
        (baker (create-place 'baker))
        (next-house (create-place 'next-house))
        (legal-seafood (create-place 'legal-seafood))
        (graduation-stage (create-place 'graduation-stage)))
    
    ;; set up heaven
    (set! heaven (create-place 'heaven))
    
    ; Connect up places
    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways grendels-den 'up 'down 32-123)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways starbucks 'east 'west student-center) 
    (can-go-both-ways lobby-7 'west 'east student-center)
    (can-go-both-ways student-center 'south 'north bexley)
    (can-go-both-ways bexley 'west 'east baker)
    (can-go-both-ways baker 'west 'east next-house)
    (can-go-both-ways lobby-10 'north 'south building-13)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways building-13 'north 'south edgerton-hall)
    (can-go-both-ways edgerton-hall 'up 'down 34-301)
    (can-go-both-ways 34-301 'up 'down eecs-hq)
    (can-go-both-ways 34-301 'east 'west stata-center)
    (can-go-both-ways stata-center 'down 'up 32-123)
    (can-go-both-ways eecs-hq 'west 'east eecs-ug-office)
    (can-go-both-ways edgerton-hall 'north 'south legal-seafood)
    (can-go-both-ways eecs-hq 'up 'down 6001-lab)
    (can-go-both-ways legal-seafood 'east 'west great-court)
    (can-go-both-ways great-court 'up 'down graduation-stage)
    
    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-mobile-thing 'tons-of-code baker)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'coffee starbucks)
    (create-mobile-thing 'coffee starbucks)
    (create-mobile-thing 'coffee starbucks)
    (create-mobile-thing 'coffee starbucks)
    (create-mobile-thing 'coffee starbucks)
    (create-mobile-thing 'coffee starbucks)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (create-mobile-thing 'engineering-book barker-library)
    (create-mobile-thing 'diploma graduation-stage)
    
    (list 10-250 lobby-10 grendels-den barker-library lobby-7
          eecs-hq eecs-ug-office edgerton-hall 34-301 6001-lab
          building-13 great-court stata-center student-street
          student-center bexley baker next-house legal-seafood
          graduation-stage)))

; all hacks exist in the bowels-of-stata.  When placing a hack
; in the outside world, the original hack from the bowels-of stata
; is cloned (using clone-hack; see objtypes.scm).
; There are no entrances, exits, or people in the bowels, preventing
;  the hacks there from being stolen.

;; Problem 4
;; Re-write hack code so that hacks only apply to people
;; Add REDBULL hack: redbull restores the health of everyone 
;; in the room, and destroys objects in the room with a 
;; probability of 1/3
(define (instantiate-hacks)
  (let ((bowels (create-place 'bowels-of-stata)))
    (let ((sp1
           (create-hack
            'BOIL
            bowels
            "hmm, I think there's something on your nose"
            (lambda (hacker target)
              (if (ask target 'IS-A 'PERSON)
                  (ask target 'EMIT
                       (list (ask target 'NAME) "grows boils on their nose"))
                  (error "I only work on people!")))))
          (sp2
           (create-hack
            'SLUG-HACK
            bowels
            "want to join the Sydney Linux Users Group?"
            (lambda (hacker target)
              (if (ask target 'IS-A 'PERSON)
                  (begin
                    (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
                    (create-mobile-thing 'SLUG (ask target 'LOCATION)))
                  (error "I only work on people!")))))
          (sp3
           (create-hack 
            'REDBULL
            bowels
            "redbull for everyone and everything!"
            (lambda (hacker target)
              (let* ((room (ask target 'LOCATION))
                     (people (find-all room 'PERSON))
                     (things (filter (lambda (x) (not (ask x 'IS-A 'PERSON))) (ask room 'THINGS))))
                (for-each (lambda (person) (ask person 'RESET-HEALTH)) people)
                (for-each (lambda (thing) (if (= (random 1) 3) (ask thing 'DESTROY))) things)))))
          
          (sp4 
           (create-hack
            'TUNNEL
            bowels
            "Connection!"
            (lambda (hacker target)
              (let* ((directions '(up down east west north south))
                     (hacker-location (ask hacker 'LOCATION))
                     (target-location (ask target 'LOCATION))
                     (hacker-direction (car (filter (lambda (direction) (not (ask (ask hacker 'LOCATION) 'EXIT-TOWARDS direction))) directions)))
                     (target-direction (car (filter (lambda (direction) (not (ask (ask target 'LOCATION) 'EXIT-TOWARDS direction))) directions))))
                (can-go-both-ways hacker-location hacker-direction target-location target-location))))))
      bowels)))

(define (populate-hacks rooms)
  (for-each (lambda (room)
              (cond ((= (random 3) 0)
                     (clone-hack (pick-random (ask bowels-of-stata 'THINGS)) room)
                     (ask screen 'TELL-WORLD (list "hack installed at" (ask room 'NAME))))))
            rooms))

(define (populate-players rooms)
  (let* ((students (map (lambda (name)
			  (create-autonomous-person name
						    (pick-random rooms)
						    (random-number 5)
						    (random-number 5)))
			'(ben-bitdiddle alyssa-hacker
			  course-6-frosh lambda-man)))
         ;uncomment after writing professors
	 (profs (map (lambda (name)
		       (create-professor name
			  	     (pick-random rooms)
                                    3
                                   3))
		     '(rob-miller eric-grimson)))
         ; uncomment after writing president
         (president (map (lambda (name)
                           (create-president name
                                             (pick-random rooms)
                                             (random-number 1)
                                             (random-number 3)))
                         '(susan-hockfield)))                                  
	 (house-masters (map (lambda (name)
                               (create-house-master name
                                                    (pick-random rooms)
                                                    (random-number 3)
                                                    (random-number 3)))
                             '(dr-evil mr-bigglesworth)))
	 (trolls (map (lambda (name)
			(create-troll name
				      (pick-random rooms)
				      (random-number 3)
				      (random-number 3)))
		      '(grendel registrar))))

    (append students
	    profs        ;uncomment after writing professor
            president    ;uncomment after writing president
	    house-masters trolls)))

(define me 'will-be-set-by-setup)
(define all-rooms 'will-be-set-by-setup)
(define bowels-of-stata 'will-be-set-by-setup)

(define (setup name)
  (ask our-clock 'RESET)
  (ask our-clock 'ADD-CALLBACK
       (create-clock-callback 'TICK-PRINTER our-clock 'PRINT-TICK))
  (let ((rooms (create-world)))
    (set! bowels-of-stata (instantiate-hacks))

    (populate-hacks rooms)

    (populate-players rooms)

    (set! me (create-avatar name (pick-random rooms)));)); (pick-random rooms)))
    (ask screen 'SET-ME me)
    (set! all-rooms rooms)
    'ready))

;; Some useful example expressions...

; (setup 'ben-bitdiddle)
; (run-clock 5)
; (ask screen 'DEITY-MODE #f)
; (ask screen 'DEITY-MODE #t)
; (ask me 'LOOK-AROUND)
; (ask me 'TAKE (thing-named 'ENGINEERING-BOOK))
; (ask me 'GO 'up)
; (ask me 'GO 'down)
; (ask me 'GO 'north)

;; Problem 2
;;--------------------
;; GPS-tracker
;;
;; A device that can tell you what rooms are occupied and 
;; what room each person is in
(define (create-GPS-tracker name location) 
  ; symbol, location -> GPS-tracker
  (create-instance GPS-tracker name location))

(define (GPS-tracker self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler
     'GPS-TRACKER
     (make-methods
      'OCCUPIED
      (lambda ()
        (for-each (lambda (x) (newline) (display (ask x 'NAME)))
                  (filter (lambda (room) (not (null? (find-all room 'PERSON)))) all-rooms)))
      'WHO-WHERE
      (lambda ()
        (for-each (lambda (person) (display-message (list (ask person 'NAME) "is at" (ask (ask person 'LOCATION) 'NAME)))) (all-people))))
     mobile-thing-part)))

;; Problem 3
;;--------------------
;; PDA
;;
;; A device that is necessary to deploy hacks

(define (create-PDA name location) 
  ; symbol, location -> PDA
  (create-instance PDA name location))

(define (PDA self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler 
     'PDA
     (make-methods
      'GOTCHA
      (lambda (target)
        (let* ((owner (ask self 'LOCATION))
               (hacks (if (ask owner 'IS-A 'person) 
                          (ask owner 'HAS-A 'HACK) 
                          (error "Can't work without an owner!"))))
          (if (null? hacks)
              (error "No hacks available.")
              (let ((hack (pick-random hacks)))
                (ask owner 'SAY (list "Deploying hack, just you wait."))
                (ask hack 'USE owner target)))))
      
      'WHATS-THIS-DO
      (lambda (hack)
        (let* ((owner (ask self 'LOCATION))
               (target (if (ask owner 'IS-A 'person)
                           (pick-random (delq owner (ask (ask owner 'LOCATION) 'THINGS)))
                           (error "Can't work without an owner!"))))
          (if (null? target)
              (error "Nothing to target in here")
              (begin
                (ask owner 'SAY (list "Deploying hack, just you wait."))
                (ask hack 'USE owner target)))))
     
     'LOOPEM
     (lambda (target)
       (let* ((owner (ask self 'LOCATION))
               (hacks (if (ask owner 'IS-A 'person) 
                          (ask owner 'HAS-A 'HACK) 
                          (error "Can't work without an owner!"))))
          (if (null? hacks)
              (error "No hacks available.")
              (for-each (lambda (hack)
                          (begin
                            (ask owner 'SAY (list "Deploying hack, just you wait."))
                            (ask hack 'USE owner target)))
                        hacks)))))
      mobile-thing-part)))

;; Problem 5
;;--------------------
;; Food
;;
;; Food restores the health of a person to its initial level
;; and then disappears

(define (create-food name location) 
  ; symbol, location -> PDA
  (create-instance food name location))

(define (food self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler 
     'food
     (make-methods
      'eat
      (lambda ()
        (let ((owner (ask self 'LOCATION)))
          (if (not (ask owner 'IS-A 'PERSON))
              (error "Only people can eat food!")
              (begin 
                (ask owner 'RESET-HEALTH)
                (ask self 'DESTROY))))))
     mobile-thing-part)))

;; Problem 6
;;--------------------
;; Professor
;;
;; Food restores the health of a person to its initial level
;; and then disappears
(define (create-professor name birthplace speed hunger)
  (create-instance professor name birthplace speed hunger))

(define (professor self name birthplace speed hunger)
  (let ((auto-part (autonomous-person self name birthplace speed 10)))
    (make-handler
     'PROFESSOR
     (make-methods
      
      'INSTALL
      (lambda ()
	(ask auto-part 'INSTALL)
	(ask our-clock 'ADD-CALLBACK
	     (create-clock-callback 'MOVE-AND-TAKE-STUFF self
				    'MOVE-AND-TAKE-STUFF)))
      
      'AWARD-GRADE
      (lambda (student)
        (if (ask self 'HAS-A-THING-NAMED 'PROBLEM-SET)
            (begin 
              (ask student 'take (create-grade 'GRADE (ask self 'LOCATION)))
              (ask self 'SAY (list "I'll pass you this time.")))))
      
      'AWARD-UROP
      (lambda (student)
        (if (ask self 'HAS-A-THING-NAMED 'TONS-OF-CODE)
            (ask student 'take (create-urop-experience 'UROP (ask self 'LOCATION)))))
      
      'INGEST-COFFEE
      (lambda ()
        (if (ask self 'HAS-A-THING-NAMED 'COFFEE)
            ('CHANGE-RESTLESSNESS))))
      auto-part)))

(define (create-grade name location) 
  ; symbol, location -> grade
  (create-instance grade name location))

(define (GRADE self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler 
     'GRADE
     (make-methods)
    mobile-thing-part)))

(define (create-urop-experience name location) 
  ; symbol, location -> grade
  (create-instance urop-experience name location))

(define (urop-experience self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler 
     'urop-experience
     (make-methods)
    mobile-thing-part)));;--------------------

(define (create-coffee name location) 
  ; symbol, location -> grade
  (create-instance coffee name location))

(define (COFFEE self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler
     'COFFEE
     (make-methods)
    mobile-thing-part)))
        
      
;; Problem 7
;;--------------------
;; President
;;
(define (create-president name birthplace speed hunger)
  (create-instance president name birthplace speed hunger))

(define (president self name birthplace speed hunger)
  (let ((auto-part (autonomous-person self name birthplace speed 10)))
    (make-handler
     'PRESIDENT
     (make-methods
      'INSTALL
      (lambda ()
	(ask auto-part 'INSTALL)
	(ask our-clock 'ADD-CALLBACK
	     (create-clock-callback 'MOVE-AND-TAKE-STUFF self
				    'MOVE-AND-TAKE-STUFF)))
      'AWARD-DIPLOMA
      (lambda (student)
        (if (and (ask self 'HAS-A 'GRADE)
                 (ask self 'HAS-A 'urop-experience))
            (begin 
              (ask student 'take (create-diploma 'DIPLOMA (ask self 'LOCATION)))
              (ask self 'SAY (list "You graduate."))))))
      auto-part)))


(define (create-diploma name location) 
  ; symbol, location -> grade
  (create-instance diploma name location))

(define (DIPLOMA self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler 
     'DIPLOMA
     (make-methods)
    mobile-thing-part)))

;; Problem 8
;;--------------------
;; Teleporter
;; Device that intanstly transports the target to the location of the hacker 
(define (create-teleporter name location) 
  ; symbol, location -> teleporter
  (create-instance teleporter name location))

(define (teleporter self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler 
     'teleporter
     (make-methods
      'teleport
      (lambda (hacker target-name)
        (let ((target (car (filter (lambda (person) (eq? (ask person 'NAME) target-name)) (all-people))))
              (hacker-location (ask hacker 'LOCATION)))
          (ask target 'CHANGE-LOCATION hacker-location))))
     mobile-thing-part)))



