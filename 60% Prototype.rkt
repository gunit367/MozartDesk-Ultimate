#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)
(require rsound/piano-tones)
(require "world-save.rkt")

;;world definition includes:
; -a complete list of the world's notes
; -the tempo of the world
; -the current beat that the world is on
; -the mode state of the world, including whether it's played or paused
; -the currently selected icon to place on the grid
; -the current page (Currently not in use)
; (make-world (list frames number string string number)
(define-struct world (worldlist tempo curbeat modestate selected page) #:transparent)

; initial state of the world for the program
(define default_list empty)
(define INITIAL_WORLD (make-world default_list 44100 0 "paused" "piano" 1))

; a note is a structure that includes
; -the type of sound
; -the pitch of a sound
; -the beat of a sound
; (make-note (string number number)
(define-struct note (type pitch beat) #:transparent)


; required function
(define (both a b) b)
(define-struct posn [x y])

; page button image
(define arrow_right (bitmap/file "graphics/arrow.png")) ;bitmap function to load from file to be implemented
(define arrow_left (rotate 180 arrow_right))

; option button image
(define options_button (bitmap/file "graphics/optionsbutton.png"))

; reset button image
(define resetbutton
  (place-image (text "RESET" 24 'blue)
               50 50
               (square 100 'solid 'red)))

; play button image
(define playbutton
  (place-image
   (rotate 270 (isosceles-triangle 70 35 'solid 'green))
   50 50
   (square 100 'solid 'black)))

; the initial rectangle design, represent empty note (no sound will be played for that rectangle when play button is clicked)
(define (button) (rectangle BEAT_WIDTH INTERVAL_HEIGHT "outline" "black"))

; s is a number
; number -> number
; s function takes in a second (time) and produces the frame of that second
(define (s n)
  (* 44100 n))

; Basic world dimensions
(define WORLD_HEIGHT 900)
(define WORLD_WIDTH 1200)

; the rectangles' width and height
(define INTERVAL_HEIGHT (/ (/ WORLD_HEIGHT 2) 8))
(define BEAT_WIDTH (/ WORLD_WIDTH 8))

; constant definition for the beat and position of staff
(define BEATS_PER_PAGE 16)
(define MIDDLE_OF_STAFF_V (/ WORLD_HEIGHT 2))
(define TOP_OF_STAFF (- MIDDLE_OF_STAFF_V (* 4 INTERVAL_HEIGHT))) ; y coordinate of the top of the staff
(define BOTTOM_OF_STAFF (+ TOP_OF_STAFF (* 8 INTERVAL_HEIGHT)))
(define START_OF_STAFF 0) ; x coordinate of the far left side of the staff
(define END_OF_STAFF (+ START_OF_STAFF (* 8 BEAT_WIDTH)))


; positions of all buttons
(define resetbuttonpos (make-posn 1000 100))
(define leftarrowpos (make-posn 675 100))
(define rightarrowpos (make-posn (+ (posn-x leftarrowpos)
                                    (image-width arrow_left))
                                 (posn-y leftarrowpos)))
(define playbuttonpos (make-posn 700 800))
(define optionsbuttonpos (make-posn 100 750))
(define buttonrowpos (make-posn 50 50)) ; pos of far left top of button row

; size of buttons
(define soundbuttonside 30)

; sound buttons
(define soundbuttonbackground (square soundbuttonside "solid" "lime"))
(define pianobutton (overlay (text "P" soundbuttonside "black")
                             soundbuttonbackground))
(define tempbutton (overlay (text "#" soundbuttonside "black")
                            soundbuttonbackground))
(define erasebutton (overlay (text "E" soundbuttonside "black")
                             soundbuttonbackground))
(define buttonrow (beside pianobutton
                          tempbutton
                          tempbutton
                          tempbutton
                          tempbutton
                          tempbutton
                          erasebutton))


; world -> world
; Draws world from structure given by big bang
; Draws the sound grid, the play button, reset button, page buttons, and the texts
(define (renderfn w)
  (makescene (world-worldlist w) (world-page w) w))

; list number world -> image
; Creates a image depending on the worldlist's list of notes
(define (makescene lon page w)
  (cond
    [(empty? lon) 
     (place-image/align (rendercols) START_OF_STAFF MIDDLE_OF_STAFF_V
                        "left" "middle"
                                 (place-image resetbutton (posn-x resetbuttonpos) (posn-y resetbuttonpos)
                                              (place-image (text "Page" 24 "indigo") 705 70
                                              (place-image arrow_right (posn-x rightarrowpos) (posn-y rightarrowpos)
                                                           (place-image arrow_left (posn-x leftarrowpos) (posn-y rightarrowpos) 
                                                                        (place-image playbutton
                                                                                     (posn-x playbuttonpos) (posn-y playbuttonpos)
                                                                                     (place-image options_button (posn-x optionsbuttonpos) (posn-y optionsbuttonpos)
                                                                                                  (place-image/align buttonrow (posn-x buttonrowpos) (posn-y buttonrowpos)
                                                                                                                     "left" "top"
                                                                        (empty-scene WORLD_WIDTH WORLD_HEIGHT)))))))))]                
    [(cons? lon) (cond [(string=? "piano" (note-type (first lon))) (place-image (rectangle BEAT_WIDTH INTERVAL_HEIGHT "solid" "red") (beatlookup (note-beat (first lon)) page) (pitchlookup (note-pitch (first lon))) (makescene (rest lon) page w))]
                       [(string=? "temp" (note-type (first lon))) (place-image (rectangle BEAT_WIDTH INTERVAL_HEIGHT "solid" "blue") (beatlookup (note-beat (first lon)) page) (pitchlookup (note-pitch (first lon))) (makescene (rest lon) page w))]
                       [else (makescene (rest lon) page w)])]))

; list number -> boolean
; checks if notes exist for a certain page
(define (notes-exist? lon page)
  (cond 
    [(empty? lon) #f]
    [(cons? lon) (or (< (* page 1 ) (note-beat (first lon)) (* page 8)) (notes-exist? (rest lon) page))]))

; the row of rectangles
(define (rendercols) 
  (beside (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)))

; the column of rectangles
(define (colrender) 
  (above (button)
          (button)
          (button)
          (button)
          (button)
          (button)
          (button)
          (button)))


; number -> number (posn-y)
; looks at the pitch of the note and determines the v-location in which it should go
(define (pitchlookup pitch)
  (cond
 [(= pitch 60) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 8)) (round (/ INTERVAL_HEIGHT 2)))] 
 [(= pitch 62) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 7)) (round (/ INTERVAL_HEIGHT 2)))] 
 [(= pitch 64) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 6)) (round (/ INTERVAL_HEIGHT 2)))] 
 [(= pitch 65) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 5)) (round (/ INTERVAL_HEIGHT 2)))] 
 [(= pitch 67) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 4)) (round (/ INTERVAL_HEIGHT 2)))] 
 [(= pitch 69) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 3)) (round (/ INTERVAL_HEIGHT 2)))] 
 [(= pitch 71) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 2)) (round (/ INTERVAL_HEIGHT 2)))] 
 [(= pitch 72) (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT 1)) (round (/ INTERVAL_HEIGHT 2)))])) 

; number number -> number (posn-x)
; looks at the beat of the note and determines the h-location in which it should go
(define (beatlookup beat page)
  (cond
    [(= beat 1) (- (+ START_OF_STAFF (* BEAT_WIDTH 1)) (round (/ BEAT_WIDTH 2)))]
[(= beat 2) (- (+ START_OF_STAFF (* BEAT_WIDTH 2)) (round (/ BEAT_WIDTH 2)))]
[(= beat 3) (- (+ START_OF_STAFF (* BEAT_WIDTH 3)) (round (/ BEAT_WIDTH 2)))]
[(= beat 4) (- (+ START_OF_STAFF (* BEAT_WIDTH 4)) (round (/ BEAT_WIDTH 2)))]
[(= beat 5) (- (+ START_OF_STAFF (* BEAT_WIDTH 5)) (round (/ BEAT_WIDTH 2)))]
[(= beat 6) (- (+ START_OF_STAFF (* BEAT_WIDTH 6)) (round (/ BEAT_WIDTH 2)))]
[(= beat 7) (- (+ START_OF_STAFF (* BEAT_WIDTH 7)) (round (/ BEAT_WIDTH 2)))]
[(= beat 8) (- (+ START_OF_STAFF (* BEAT_WIDTH 8)) (round (/ BEAT_WIDTH 2)))]))


; Mouse Event Handler
; World X Y Event -> World

; Returns true if mouse is within r pixels from the point
; Number Number Number Number Number -> Boolean
(define (mousewithin mouse-x mouse-y point-x point-y xr yr)
  (or (< (abs (- mouse-x point-x)) xr)
  (< (abs (- mouse-y point-y)) yr)))

(define (mousewithin-1dir mouse-xory point-xory r)
  (< (abs (- mouse-xory point-xory)) r))


;;These functions find the row or column that the mouse is in
; number -> number (row)
; checks which row the mouse coordinates are in
(define (mouserow y)
  (ceiling (/ (- y TOP_OF_STAFF) INTERVAL_HEIGHT)))

; number number -> number (column)
; check which column the mouse coordinates are in
(define (mousecol x page)
  (+ (* (- page 1) 8)(ceiling (/ (- x START_OF_STAFF) BEAT_WIDTH))))


; number number string number number -> note (structure)
; Decides what to set the fields as in each new note added to the worldlist
(define (new-note col row selected beat page)
  (make-note selected (findpitch row) (findbeat col beat page)))

; number -> number (pitch)
; each row represent a pitch, like row 8 represent pitch 60
(define (findpitch row)
  (cond [(= row 8) 60]
        [(= row 7) 62]
        [(= row 6) 64]
        [(= row 5) 65]
        [(= row 4) 67]
        [(= row 3) 69]
        [(= row 2) 71]
        [(= row 1) 72]))

; number number number -> number (beat)
; find the current beat and use it to tell which column the program is suppose to move to and play that column's sound
(define (findbeat col beat page)
 (* page (+ col (* BEATS_PER_PAGE (floor (/ beat BEATS_PER_PAGE))))))

; posn posn -> boolean
; checks if the mouse coordinates are on staff
(define (mouseonstaff? x y)
  (and (> y TOP_OF_STAFF)
       (< y BOTTOM_OF_STAFF)
       (> x START_OF_STAFF)
       (< x END_OF_STAFF)))

; posn posn -> boolean
; checks if the mouse coordinates are on any f the sound buttons
(define (mouseonbuttons? x y)
  (and (> x (posn-x buttonrowpos))
       (< x (+ (posn-x buttonrowpos) (* soundbuttonside 7)))
       (> y (posn-y buttonrowpos))
       (< y (+ (posn-y buttonrowpos) soundbuttonside))))

; posn posn -> boolean
; checks if the mouse coordinates are on the reset button
(define (mouseonreset? x y)
  (and (> y (- (posn-y resetbuttonpos) (/ (image-height resetbutton) 2)))
       (< y (+ (posn-y resetbuttonpos) (/ (image-height resetbutton) 2)))
       (> x (- (posn-x resetbuttonpos) (/ (image-width resetbutton) 2)))
       (< x (+ (posn-x resetbuttonpos) (/ (image-width resetbutton) 2)))))

; posn posn -> boolean
; checks if the mouse coordinates are on the play button
(define (mouseonplay? x y)
  (and (> y (- (posn-y playbuttonpos) (/ (image-height playbutton) 2)))
       (< y (+ (posn-y playbuttonpos) (/ (image-height playbutton) 2)))
       (> x (- (posn-x playbuttonpos) (/ (image-width playbutton) 2)))
       (< x (+ (posn-x playbuttonpos) (/ (image-width playbutton) 2)))))

; world posn posn -> world
; checks which sound buttons the mouse coordinates are on and creates a world with a specific world-selected correspond to that specific sound button
(define (buttonrowfunc w x y)
  (cond [(and (> x (posn-x buttonrowpos))   ;first button
              (< x (+ (posn-x buttonrowpos) soundbuttonside))
              (> y (posn-y buttonrowpos))
              (< y (+ (posn-y buttonrowpos) soundbuttonside))) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) "piano" (world-page w))]
        [(and (> x (+ (posn-x buttonrowpos) (* 1 soundbuttonside 1)))   ; second
              (< x (+ (posn-x buttonrowpos) (* 2 soundbuttonside)))
              (> y (posn-y buttonrowpos))
              (< y (+ (posn-y buttonrowpos) soundbuttonside))) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) "temp" (world-page w))]
        [(and (> x (+ (posn-x buttonrowpos) (* 2 soundbuttonside 1)))   
              (< x (+ (posn-x buttonrowpos) (* 3 soundbuttonside)))
              (> y (posn-y buttonrowpos))
              (< y (+ (posn-y buttonrowpos) soundbuttonside))) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) "temp" (world-page w))]
        [(and (> x (+ (posn-x buttonrowpos) (* 3 soundbuttonside 1)))   
              (< x (+ (posn-x buttonrowpos) (* 4 soundbuttonside)))
              (> y (posn-y buttonrowpos))
              (< y (+ (posn-y buttonrowpos) soundbuttonside))) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) "temp" (world-page w))]
        [(and (> x (+ (posn-x buttonrowpos) (* 4 soundbuttonside 1)))   
              (< x (+ (posn-x buttonrowpos) (* 5 soundbuttonside)))
              (> y (posn-y buttonrowpos))
              (< y (+ (posn-y buttonrowpos) soundbuttonside))) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) "temp" (world-page w))]
        [(and (> x (+ (posn-x buttonrowpos) (* 5 soundbuttonside 1)))   
              (< x (+ (posn-x buttonrowpos) (* 6 soundbuttonside)))
              (> y (posn-y buttonrowpos))
              (< y (+ (posn-y buttonrowpos) soundbuttonside))) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) "temp" (world-page w))]
        [(and (> x (+ (posn-x buttonrowpos) (* 6 soundbuttonside 1)))   
              (< x (+ (posn-x buttonrowpos) (* 7 soundbuttonside)))
              (> y (posn-y buttonrowpos))
              (< y (+ (posn-y buttonrowpos) soundbuttonside))) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) "erase" (world-page w))]))




; Handles the mouse for a button-down event
; world x y event -> world
; By pressing the block, the block will turn from outline to a colored solid block, depended on the current world-selected
; By pressing the reset button, the block will all turn back to outline
; By pressing the play button, the program will play the rsound depending on which blocks are currently red solid
(define (mousefn w x y evt)
  (cond [(and (mouseonstaff? x y) (string=? evt "button-down")) (buttondownhandler w x y)] ;(buttondownhandler w x y)]
       [(and (mouseonreset? x y) (string=? evt "button-down")) INITIAL_WORLD]
       [(and (mouseonplay? x y) (string=? evt "button-down")) (both (play (make-song (world-worldlist w) (world-tempo w))) w)]
       ;[(and (mouseonleftpage? x y) (string=? evt "button-down")) w]
       ;[(and (mouseonrightpage? x y) (string=? evt "button-down")) w]
       [( and (mouseonbuttons? x y) (string=? evt "button-down")) (buttonrowfunc w x y)]
        [else w]))

; number string -> number
; changes the page of the composer
; this function ensures the page doesn't below the value of one
(define (change-page page navtype)
  (cond 
    [(and (string=? navtype "back") (= page 1)) 1]
    [(string=? navtype "forward") (add1 page)]
    [(string=? navtype "back") (sub1 page)]))

; world number number -> world
; check if there is note for a rectangle and either
; -adds a note if there is no note for that rectangle currently
; -erases a note if there is already a note exists for that rectangle currently
; handle event is mouse clicks on the staff
(define (buttondownhandler w x y)
  (cond 
    [(and (note-exists? x y (mousecol x (world-page w)) (world-worldlist w) (world-page w)) (string=? (world-selected w) "eraser")) (delete-note x y (world-page w) (world-worldlist w))]
    [else (make-world (cons (new-note (mousecol x (world-page w)) (mouserow y) (world-selected w) (world-curbeat w) (world-page w)) (world-worldlist w))
                                        (world-tempo w) (world-curbeat w) (world-modestate w) (world-selected w) (world-page w))]))

; number number number list number - > boolean
; checks if there is a note at posn-x and posn-y?
(define (note-exists? x y beat lon page)
  (cond
    [(empty? lon) #f]
    [else (or (equal? (make-note (mousecol x page) (mouserow y) beat) (first lon)) (note-exists? x y beat (rest lon) page))]))

; world number number list -> note (structure)
; deletes note from the worldlist
(define (delete-note w x y lon)
 (remove (make-note "don'tcare" (findpitch (mouserow y)) (mousecol x (world-page w)) (lambda (pitch beat note) 
                                                                                                  (and (= pitch (note-pitch note)) (= beat (note-beat note)))))))


; note -> list with rsound and play time
; turn a note into a sound and a time to be useed in the make-song function
(define (make-note+time n tempo)(cond
                       [(string=? (note-type n) "piano") (list (piano-tone (note-pitch n)) (* tempo (note-beat n)))]))

; list-of-notes tempo -> list of (list sound time)
; turns a list of notes into a list of list of sounds and times for the assemble function
 (define (make-notes+times lon tempo)(cond
                                 [(empty? lon) empty]
                                 [else (cons (make-note+time (first lon) tempo) (make-notes+times (rest lon) tempo))]
                                 ))


; list-of-notes -> rsound
; takes a list of notes and turns them into one big sound
(define (make-song lon tempo) (assemble (make-notes+times lon tempo)))

; world -> world
; checks the modestate and either
; -play the rsound if the modestate is "playing"
; -pause the rsound if the modestate is "paused"
(define (tick w)
  (cond [(string=? (world-modestate w) "playing")(make-world (world-worldlist w) (world-tempo w) (+ (world-curbeat w) (* 1/28 (world-tempo w))) (world-modestate w) (world-selected w)(world-page w))]
        [(string=? (world-modestate w) "paused") w]))

; used to stop the sound for the tick function for right now
; (stop)

;;magic-write function for saving and loading the world
(define maker-table
  (list (list "world" make-world)))

(define (string->structs w)
  (string->struct/maker maker-table w))

;(string->structs (write-to-string INITIAL_WORLD)) result: (world '() 44100 0 "paused" "piano" 1)
;(write-to-string INITIAL_WORLD) result: "#(struct:world () 44100 0 \"paused\" \"piano\" 1)"

#;(define (savefile w x y)
  (write-to-string (buttondownhandler w x y)))
  
#;(define (loadfile w x y)
  (string->structs (savefile w x y)))

;;Reference
; (define-struct world (worldlist tempo curbeat modestate selected page))
; (define default_list empty)
; (define INITIAL_WORLD (make-world default_list 44100 0 "paused" "piano" 1))
(big-bang INITIAL_WORLD 
          [on-mouse mousefn]
          [on-draw renderfn]
          [on-tick tick]
          )
