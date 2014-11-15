#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)
(require rsound/piano-tones)

; required function
(define (both a b) b)

; s is a number
; number -> number
; s function takes in a second(time) and produces the frame of that second
(define (s n)
  (* 44100 n))

;world definition includes:
; -a complete list of the world's notes
; -the tempo of the world
; -the current beat that the world is on
; -the mode state of the world, including whether it's played or paused
; -the currently selected icon to place on the grid
; -the current page (Currently not in use)
; (make-world (list frames number string string number)
(define-struct world (worldlist tempo curbeat modestate selected page))

; initial state of the world for the program
(define default_list empty)
(define INITIAL_WORLD (make-world default_list 44100 0 "edit" "piano" 0))


; a note is a structure that includes
; -the type of sound
; -the pitch of a sound
; -the beat of a sound
; (make-note (string number number)
(define-struct note (type pitch beat))


; the square's width and height
(define INTERVAL_HEIGHT 75)
(define BEAT_WIDTH 75)

; constant definition for the beat
(define BEATS_PER_PAGE 16)
(define TOP_OF_STAFF 0) ; y coordinate of the top of the staff
(define BOTTOM_OF_STAFF (+ TOP_OF_STAFF (* 8 INTERVAL_HEIGHT)))
(define START_OF_STAFF 0) ; x coordinate of the far left side of the staff
(define END_OF_STAFF (+ START_OF_STAFF (* 8 BEAT_WIDTH)))


; page button arrows, default at page one
; page buttons are not functional for right now since we are only using on page for prototype
(define arrow_left empty-image)

(define arrow_right .)


(define ps (make-pstream))

;plays all the notes in a list of notes 
(define (play-notes lon tempo startframe) 
 (cond [(empty? lon) ps] 
      [else 
      (both (play-note (first lon) tempo startframe)  
             (play-notes (rest lon) tempo startframe))])) 


;; play a single note, based on what kind of sound it is along with it's time and pitch 
;; note -> nothing? 
(define (play-note n tempo startframe)
  (pstream-queue ps (piano-tone (note-pitch n)) (+ startframe (* tempo (note-beat n)))))


; world -> image
; Draws world from structure given by big bang
; Draws the sound grid, the play button, reset button, page buttons, and the texts
(define (renderfn w)
  (makescene (world-worldlist w)))

; list -> image
; Creates a image depending on the worldlist's list of notes
(define (makescene lon)
  (cond
    [(empty? lon) 
     (overlay/align "left" "top" (rendercols) 
                                 (place-image resetbutton 700 300
                                              (place-image (text "Page" 24 "indigo") 750 20
                                              (place-image arrow_right 800 100 
                                                           (place-image arrow_left 700 100 
                                                                        (place-image play-button
                                                                                     700 500
                                                                        (empty-scene 900 700)))))))]                
    [(cons? lon) (place-image (rectangle 75 75 "solid" "red") (beatlookup (note-beat (first lon))) (pitchlookup (note-pitch (first lon))) (makescene (rest lon)))]))

; the row of rectangle
(define (rendercols) 
  (beside (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)))

; the column of rectangle
(define (colrender) 
  (above (button)
          (button)
          (button)
          (button)
          (button)
          (button)
          (button)
          (button)))

; the rectangle design
(define (button) (rectangle BEAT_WIDTH INTERVAL_HEIGHT 'outline 'black))

                 
; looks at the pitch of the note and determines the v-location in which it should go
(define (pitchlookup pitch)
  (cond
    [(= pitch 60) (+ TOP_OF_STAFF 562)]
    [(= pitch 62) (+ TOP_OF_STAFF 487)]
    [(= pitch 64) (+ TOP_OF_STAFF 412)]
    [(= pitch 65) (+ TOP_OF_STAFF 337)]
    [(= pitch 67) (+ TOP_OF_STAFF 262)]
    [(= pitch 69) (+ TOP_OF_STAFF 187)]
    [(= pitch 71) (+ TOP_OF_STAFF 112)]
    [(= pitch 72) (+ TOP_OF_STAFF 37)]))
  
; looks at the beat of the note and determines the h-location in which it should go
(define (beatlookup beat)
  (cond
    [(= beat 1) (+ START_OF_STAFF 37)]
    [(= beat 2) (+ START_OF_STAFF 112)]
    [(= beat 3) (+ START_OF_STAFF 187)]
    [(= beat 4) (+ START_OF_STAFF 262)]
    [(= beat 5) (+ START_OF_STAFF 337)]
    [(= beat 6) (+ START_OF_STAFF 412)]
    [(= beat 7) (+ START_OF_STAFF 487)]
    [(= beat 8) (+ START_OF_STAFF 562)]))
                  

; reset button image
(define resetbutton
  (place-image (text "RESET" 24 'blue)
               50 50
               (square 100 'solid 'red)))

; play button image
(define play-button
  (place-image
   (rotate 270 (isosceles-triangle 70 35 'solid 'green))
   50 50
   (square 100 'solid 'black)))

; Mouse Event Handler
; World X Y Event -> World

; Returns true if mouse is within r pixels from the point
; Number Number Number Number Number -> Boolean
(define (mousewithin mouse-x mouse-y point-x point-y xr yr)
  (or (< (abs (- mouse-x point-x)) xr)
  (< (abs (- mouse-y point-y)) yr)))

(define (mousewithin-1dir mouse-xory point-xory r)
  (< (abs (- mouse-xory point-xory)) r))


; These functions find the row or column that the mouse is in
; x -> number <or> y -> number
(define (mouserow y)
  (ceiling (/ (- y TOP_OF_STAFF) INTERVAL_HEIGHT)))

(define (mousecol x)
  (ceiling (/ (- x START_OF_STAFF) BEAT_WIDTH)))


; Decides what to set the fields as in each new note added to the worldlist
; Beat Pitch Sound -> Note
(define (new-note col row selected beat)
  (make-note selected (findpitch row) (findbeat col beat)))

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


;find the current beat an use it to tell which column the program is suppose to move to and play that column's sound
(define (findbeat col beat)
  (+ col (* BEATS_PER_PAGE (floor (/ beat BEATS_PER_PAGE)))))


; Handles the mouse for a button-down event
; world x y event -> world
; By pressing the block, the block will turn from outline to red solid block
; By pressing the reset button, the block will all turn back to outline
; By pressing the play button, the program will play the piano-tones depending on which blocks are currently red solid
(define (mousefn w x y evt)
  (cond [(and (and (< x 600) (< y 600)) (string=? evt "button-down")) (buttondownhandler w x y)]
        [(and (and (< 650 x 750) (< 250 y 350)) (string=? evt "button-down")) INITIAL_WORLD]
        [(and (and (< 650 x 750) (< 450 y 550)) (string=? evt "button-down")) 
         (both (play-notes (world-worldlist w) (world-tempo w) (pstream-current-frame ps)) w)] 
        [else w]))

(define (buttondownhandler w x y)
  (make-world (cons (new-note (mousecol x) (mouserow y) (world-selected w) (world-curbeat w)) (world-worldlist w))
                                        (world-tempo w) (world-curbeat w) (world-modestate w) (world-selected w) (world-page w)))

(big-bang INITIAL_WORLD 
          [on-mouse mousefn]
          [on-draw renderfn]
          )
