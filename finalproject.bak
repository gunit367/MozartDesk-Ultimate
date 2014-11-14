#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;world definition includes a complete list of the world's notes
;the tempo of the world
;the current beat that the world is on
;the mode state of the world
(define-struct world (worldlist tempo curbeat modestate))
(define-struct note (type pitch beat))
(define (both a b) b)
(define default_list empty)

(define INITIAL_WORLD (make-world default_list 120 0 "edit"))


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
    [(empty? lon) (overlay/align "left" "top" (rendercols) (place-image resetbutton 700 700 (empty-scene 900 900)))]                 ;0 0 (place-image resetbutton 850 850 (empty-scene 900 900)))]
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

(define (button) (square 75 'outline 'black))

                 
; looks at the pitch of the note and determines the h-location in which it should go
(define (pitchlookup pitch)
  (cond
    [(= pitch 60) 800]
    [(= pitch 61) 725]
    [(= pitch 62) 650]
    [(= pitch 63) 575]
    [(= pitch 64) 500]
    [(= pitch 65) 425]
    [(= pitch 66) 350]
    [(= pitch 67) 275]
    [(= pitch 68) 200]))
  
; looks at the beat of the note and determines the v-location in which it should go
(define (beatlookup beat)
  (cond
    [(= beat 1) 150]
    [(= beat 2) 225]
    [(= beat 3) 300]
    [(= beat 4) 375]
    [(= beat 5) 450]
    [(= beat 6) 525]
    [(= beat 7) 600]
    [(= beat 8) 675]))
                  
                  
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
(define (pause-button bool)
  (place-image
   (cond 
     [(not bool) pausebars]
     [else (rotate 270 (isosceles-triangle 70 35 'solid 'green))]
         )
   50 50
   (square 100 'solid 'black)))

(big-bang INITIAL_WORLD 
          ;[on-tick tickfn]
          ;[on-mouse mousefn]
          [on-draw renderfn]
          )
 


    
    
    
  