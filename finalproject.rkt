#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)

;world definition includes a complete list of the world's notes
;the tempo of the world
;the current beat that the world is on
;the mode state of the world, including whether it's played or paused
(define-struct world (worldlist tempo curbeat modestate selected))
(define-struct note (type pitch beat))
(define (both a b) b)
(define default_list empty)

(define INTERVAL_HEIGHT 75)
(define BEAT_WIDTH 75)

(define BEATS_PER_PAGE 16)
(define TOP_OF_STAFF 0) ; y coordinate of the top of the staff
(define BOTTOM_OF_STAFF (+ TOP_OF_STAFF (* 8 INTERVAL_HEIGHT)))
(define START_OF_STAFF 0) ; x coordinate of the far left side of the staff
(define END_OF_STAFF (+ START_OF_STAFF (* 8 BEAT_WIDTH)))

(define INITIAL_WORLD (make-world default_list 120 0 "edit" "piano"))
(define arrow_left empty-image )

(define arrow_right empty-image)




; number -> image
; Makes a slider depending on what the 
; tempo of the world is
(define (temposlider tempo)
  (above (text "Tempo" 20 'brown)
         (overlay/offset (circle 10 'solid 'blue)
                  0 (* 5 (- 15.5 tempo))
                  (rectangle 10 168 'solid 'black))))

; world -> image
; Draws world from structure given by big bang

(define (renderfn w)
  (makescene (world-worldlist w)))

     
(define (makescene lon)
  (cond
    [(empty? lon) 
     (overlay/align "left" "top" (rendercols) 
                                 (place-image resetbutton 700 700 
                                              (place-image arrow_right 775 100 
                                                           (place-image arrow_left 700 100 
                                                                        (place-image (pause-button true)
                                                                                     500 500
                                                                        (empty-scene 900 900))))))]                
    [(cons? lon) (place-image (rectangle 75 75 "solid" "red") (beatlookup (note-beat (first lon))) (pitchlookup (note-pitch (first lon))) (makescene (rest lon)))]))

(define (rendercols) 
  (beside (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)
         (colrender)))

(define (colrender) 
  (above (button)
          (button)
          (button)
          (button)
          (button)
          (button)
          (button)
          (button)))

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
                  
                  
; image
(define pausebars
  (overlay/offset (rectangle 20 70 'solid 'red)
                  30 0 (rectangle 20 70 'solid 'red)))

;image
(define resetbutton
  (place-image (text "RESET" 24 'blue)
               50 50
               (square 100 'solid 'red)))

; Boolean -> image
; if handed false, produces pausebars
; otherwise produces the playing triangle
(define (pause-button state)
  (place-image
   (cond 
     [(not state) pausebars]
     [else (rotate 270 (isosceles-triangle 70 35 'solid 'green))]
         )
   50 50
   (square 100 'solid 'black)))





;(define-struct world (worldlist tempo curbeat modestate selected))
;(define-struct note (type pitch beat))

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

(define (findpitch row)
  (cond [(= row 8) 60]
        [(= row 7) 62]
        [(= row 6) 64]
        [(= row 5) 65]
        [(= row 4) 67]
        [(= row 3) 69]
        [(= row 2) 71]
        [(= row 1) 72]))

(define (findbeat col beat)
  (+ col (* BEATS_PER_PAGE (floor (/ beat BEATS_PER_PAGE)))))

; Handles the mouse for a button-down event
; world x y event -> world
(define (buttondownhandler w x y)
  (make-world (cons (new-note (mousecol x) (mouserow y) (world-selected w) (world-curbeat w)) (world-worldlist w))
                                        (world-tempo w) (world-curbeat w) (world-modestate w) (world-selected w)))


(define (mousefn w x y evt)
  (cond [(string=? evt "button-down") (buttondownhandler w x y)]
        [else w]))

(big-bang INITIAL_WORLD 
          ;[on-tick tickfn]
          [on-mouse mousefn]
          [on-draw renderfn]
          )
 


    
    
    
  