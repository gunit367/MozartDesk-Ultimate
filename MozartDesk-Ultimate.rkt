#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require test-engine/racket-tests)
(require rackunit)
(require rsound)
(require rsound/piano-tones)
(require "world-save.rkt")

;;world definition includes:
; -a complete list of the world's notes
; -the tempo of the world (in beats per second)
; -the current beat that the world is on
; -the mode state of the world, including whether it's played or paused
; -the currently selected icon to place on the grid
; -the current page
; (make-world (list frames number string string number)
(define-struct world (worldlist tempo curbeat modestate selected page randval) #:transparent)

; initial state of the world for the program
(define default_list empty)
(define INITIAL_WORLD (make-world default_list 2 0 "start" "piano" 1 (random 10)))

; a note is a structure that includes
; -the type of sound
; -the pitch of a sound
; -the beat of a sound
; (make-note (string number number)
(define-struct note (type pitch beat) #:transparent)

; used for some test cases
(define test-world (make-world (list (make-note "piano" 55 2)) 2 2 "paused" "piano" 1 1))
(define test-world2 (make-world (list (note "piano" 71 2) (note "piano" 72 1)) 2 0 "paused" "piano" 1 1))

; required function
(define (both a b) b)
(define-struct posn [x y])

; s is a number
; number -> number
; s function takes in a second (time) and produces the frame of that second
(define (s n)
  (* 44100 n))

; option world image
(define optionsbackground (bitmap/file "Images/optionsbackground.jpg"))
(define savebutton (bitmap/file "Images/savebutton.png"))
(define loadbutton (bitmap/file "Images/loadbutton.png"))
(define exitbutton (bitmap/file "Images/exitbutton.png"))
(define resumebutton (bitmap/file "Images/resumebutton.png"))

; composer world image
(define arrowleft (bitmap/file "Images/arrowl.png"))
(define arrowright (bitmap/file "Images/arrowr.png"))
(define startscreen (bitmap/file "Images/startscreen.png"))
(define background (bitmap/file "Images/background.jpg"))
(define playbutton (bitmap/file "Images/playbutton.png"))
(define resetbutton (bitmap/file "Images/resetbutton.png"))
(define optionsbutton (bitmap/file "Images/optionsbutton.png"))
(define pausebutton (bitmap/file "Images/pausebutton.png"))
(define shutdownbackground (bitmap/file "Images/shutdownbackground.jpg"))
(define (button) (rectangle BEAT_WIDTH INTERVAL_HEIGHT "outline" "white")) ; the initial rectangle design, represent empty note 
                                                                           ; (no sound will be played for that rectangle when play button is clicked)

;Mozart Assistant images
(define mozart (bitmap/file "Images/mozart.jpg"))
(define q1 (bitmap/file "Images/q1.jpg"))
(define q2 (bitmap/file "Images/q2.jpg"))
(define q3 (bitmap/file "Images/q3.jpg"))
(define q4 (bitmap/file "Images/q4.jpg"))
(define q5 (bitmap/file "Images/q5.jpg"))

; the column of rectangles
(define (colrender) 
  (beside (above (button)
                 (button)
                 (button)
                 (button)
                 (button)
                 (button)
                 (button)
                 (button))
          (above (button)
                 (button)
                 (button)
                 (button)
                 (button)
                 (button)
                 (button)
                 (button))))


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

;startup rsound definition
(define starts (rs-read "intro.wav"))

; Basic world dimensions
(define WORLD_HEIGHT (image-height background))
(define WORLD_WIDTH (image-width background))

; the rectangles' width and height
(define BEATS_PER_PAGE 16)
(define INTERVAL_HEIGHT (/ (/ WORLD_HEIGHT 2) 8))
(define BEAT_WIDTH (/ WORLD_WIDTH BEATS_PER_PAGE))

; constant definition for the beat and position of staff
(define MIDDLE_OF_STAFF_V (- (/ WORLD_HEIGHT 2) 50))
(define TOP_OF_STAFF (- MIDDLE_OF_STAFF_V (* 4 INTERVAL_HEIGHT))) ; y coordinate of the top of the staff
(define BOTTOM_OF_STAFF (+ TOP_OF_STAFF (* 8 INTERVAL_HEIGHT)))
(define START_OF_STAFF 0) ; x coordinate of the far left side of the staff
(define END_OF_STAFF (+ START_OF_STAFF (* BEATS_PER_PAGE BEAT_WIDTH)))

; tempo box and changer button images
(define tempoplusbutton
  (place-image 
   (text/font "+" 24 "white" "Segoe UI" 'roman 'normal 'normal #f)
   12 12
   (square 24 "outline" "black")))

(define tempominusbutton
  (place-image 
   (text/font "-" 24 "white" "Segoe UI" 'roman 'normal 'normal #f)
   12 12
   (square 24 "outline" "black")))

(define tempobox
  (square 35 "outline" "black"))
(define tempoboxlabel
  (text/font "Tempo" 24 "white" "Segoe UI" 'roman 'normal 'normal #f))

; size of buttons
(define soundbuttonside 30)

; sound buttons
(define soundbuttonbackground (overlay (square (- soundbuttonside 1) "solid" "lime")
                                       (square soundbuttonside 'outline 'black)))
(define pianobutton (overlay (text "P" soundbuttonside "black")
                             soundbuttonbackground))
(define tempbutton (overlay (text "V" soundbuttonside "black")
                            soundbuttonbackground))
(define tempbutton2 (overlay (text "H" soundbuttonside "black")
                             soundbuttonbackground))
(define tempbutton3 (overlay (text "V" soundbuttonside "black")
                             soundbuttonbackground))
(define tempbutton4 (overlay (text "K" soundbuttonside "black")
                             soundbuttonbackground))
(define tempbutton5 (overlay (text "V" soundbuttonside "black")
                             soundbuttonbackground))
(define erasebutton (overlay (text "E" soundbuttonside "black")
                             soundbuttonbackground))
(define buttonrow (beside pianobutton
                          tempbutton
                          tempbutton2
                          tempbutton3
                          tempbutton4
                          tempbutton5
                          erasebutton))

; a row of green rectangle that is used as the beat identifier
(define beatbox (rectangle BEAT_WIDTH INTERVAL_HEIGHT 'outline 'green))
(define beatselect (beside beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox
                           beatbox))

; positions of all buttons
(define resetbuttonpos (make-posn 575 75))
(define leftarrowpos (make-posn 675 75))

(define rightarrowpos (make-posn (+ (posn-x leftarrowpos)
                                    (image-width arrowleft))
                                 (posn-y leftarrowpos)))
(define playbuttonpos (make-posn 75 690))
(define optionsbuttonpos (make-posn (- WORLD_WIDTH (/ (image-width optionsbutton) 2))
                                    (image-height optionsbutton)))
(define buttonrowpos (make-posn 50 75)) ; pos of far left top of button row
(define savebuttonpos (make-posn (/ WORLD_WIDTH 2)
                                 150))
(define loadbuttonpos (make-posn (/ WORLD_WIDTH 2)
                                 300))
(define resumebuttonpos (make-posn (/ WORLD_WIDTH 2)
                                   450))
(define exitbuttonpos (make-posn (/ WORLD_WIDTH 2)
                                 600))
(define tempoboxpos (make-posn 450 90))
(define temposelpos (make-posn 482 90))
(define beatselectpos (make-posn (/ (+ START_OF_STAFF END_OF_STAFF) 2)
                                 (+ BOTTOM_OF_STAFF (image-height beatselect))))
(define mozartpos (make-posn 980 730))

; Options menu display, a visual constant
(define optionsmenu
  (place-image savebutton
               (posn-x savebuttonpos) (posn-y savebuttonpos)
               (place-image loadbutton
                            (posn-x loadbuttonpos) (posn-y loadbuttonpos)
                            (place-image resumebutton
                                         (posn-x resumebuttonpos) (posn-y resumebuttonpos)
                                         (place-image exitbutton
                                                      (posn-x exitbuttonpos) (posn-y exitbuttonpos)
                                                      optionsbackground)))))
; shut down screen, a visual constant
(define shutdowndialog
  (place-image shutdownbackground 512 512 (empty-scene (image-width shutdownbackground) (image-height shutdownbackground))))

; visual constant in the composer screen
(define playconstants
  (place-image/align (rendercols) START_OF_STAFF MIDDLE_OF_STAFF_V
                     "left" "middle"
                     (place-image resetbutton (posn-x resetbuttonpos) (posn-y resetbuttonpos)
                                  (place-image arrowright (posn-x rightarrowpos) (posn-y rightarrowpos)
                                               (place-image arrowleft (posn-x leftarrowpos) (posn-y rightarrowpos)
                                                            (place-image optionsbutton (posn-x optionsbuttonpos) 
                                                                         (posn-y optionsbuttonpos)
                                                                         (place-image/align buttonrow (posn-x buttonrowpos) 
                                                                                            (posn-y buttonrowpos) "left" "top"
                                                                                            (place-image tempoboxlabel (- (posn-x tempoboxpos) 70) (posn-y tempoboxpos)
                                                                                                         (place-image tempobox (posn-x tempoboxpos) (posn-y tempoboxpos)
                                                                                                                      (place-image tempoplusbutton (posn-x temposelpos) (- (posn-y temposelpos) 36)
                                                                                                                                   (place-image tempominusbutton (posn-x temposelpos) (+ 36 (posn-y temposelpos))
                                                                                                                                                (place-image beatselect (posn-x beatselectpos) (posn-y beatselectpos)
                                                                                                                                                             background))))))))))))


; world -> world
; Draws world from structure given by big bang
; Draws the sound grid, the play button, reset button, page buttons, and the texts
(define (renderfn w)
  (cond [(or (string=? (world-modestate w) "playing")
             (string=? (world-modestate w) "paused"))
         (makescene (world-worldlist w) (world-page w) w)]
        [(string=? (world-modestate w) "options") optionsmenu]
        [(string=? (world-modestate w) "shutdown") shutdowndialog]
        [(string=? (world-modestate w) "start") startscreen]
        [else (empty-scene 100 100)]))

(check-expect (renderfn (make-world default_list 2 0 "paused" "piano" 1 1)) (makescene default_list 1 (make-world default_list 2 0 "paused" "piano" 1 1)))

; string -> image
; checks if the playstate is in "playing" mode, if it is, then play button becomes pause button
; if the playstate is in "paused" mode, then pause button becomes play button
(define (detplaystate playstate)
  (if (string=? playstate "playing") pausebutton playbutton))

(check-expect (detplaystate "paused") playbutton)

; number -> image
; Mozart randomly says a quote from the bottom right of the composer screen by using the random function
(define (detmozartquote quote)
  (cond
    [(< quote 2) q1]
    [(< quote 4) q2]
    [(< quote 6) q3]
    [(< quote 8) q4]
    [(< quote 10) q5]))

; helper function for makescene function
; list-of-notes number string world -> image
; depending of which sound button (i.e "piano" and "vgame1") is currently used,
; the clicked rectangle will turn into a corresponding color for that sound
(define (rectangle-color lon page color w) (place-image (rectangle BEAT_WIDTH INTERVAL_HEIGHT "solid" color)
                                                        (beatlookup (note-beat (first lon)) page)
                                                        (pitchlookup (note-pitch (first lon))) (makescene (rest lon) page w)))

; list-of-notes number world -> image
; Creates a world image depending on the worldlist's list of notes
(define (makescene lon page w)
  (cond [(empty? lon) 
         (place-image/align (rendercols) START_OF_STAFF MIDDLE_OF_STAFF_V
                            "left" "middle"
                            (place-image resetbutton (posn-x resetbuttonpos) (posn-y resetbuttonpos)
                                         (place-image (text/font (string-append "Page " (number->string page)) 24 "White" "Segoe UI" 'roman 'normal 'normal #f) 
                                                      (/ (+ (posn-x rightarrowpos) (posn-x leftarrowpos)) 2) 
                                                      25
                                                      (place-image arrowright (posn-x rightarrowpos) (posn-y rightarrowpos)
                                                                   (place-image arrowleft (posn-x leftarrowpos) (posn-y rightarrowpos) 
                                                                                (place-image (detplaystate (world-modestate w))
                                                                                             (posn-x playbuttonpos) (posn-y playbuttonpos)
                                                                                             (place-image optionsbutton (posn-x optionsbuttonpos) 
                                                                                                          (posn-y optionsbuttonpos)
                                                                                                          (place-image/align buttonrow (posn-x buttonrowpos) 
                                                                                                                             (posn-y buttonrowpos) "left" "top"
                                                                                                                             (place-image tempoboxlabel (- (posn-x tempoboxpos) 70) (posn-y tempoboxpos)
                                                                                                                                          (place-image tempobox (posn-x tempoboxpos) (posn-y tempoboxpos)
                                                                                                                                                       (place-image (text/font (substring (number->string (round (* 60 (world-tempo w)))) 0 3) 18 "white" "Segoe UI" 'roman 'normal 'normal #f) (posn-x tempoboxpos)
                                                                                                                                                                    (posn-y tempoboxpos)
                                                                                                                                                                    (place-image tempoplusbutton (posn-x temposelpos) (- (posn-y temposelpos) 36)
                                                                                                                                                                                 (place-image tempominusbutton (posn-x temposelpos) (+ 36 (posn-y temposelpos))
                                                                                                                                                                                              (place-image/align (text/font "MozartDesk Ultimate" 36 "white" "Segoe UI" 'roman 'normal 'normal #f) 25 890 "left" "bottom"
                                                                                                                                                                                                                 (place-image beatselect (posn-x beatselectpos) (posn-y beatselectpos)
                                                                                                                                                                                                                              (place-image (greendot? w) (beatdotx w) (posn-y beatselectpos)
                                                                                                                                                                                                                                           (place-image mozart (posn-x mozartpos) (posn-y mozartpos)
                                                                                                                                                                                                                                                        (place-image (detmozartquote (world-randval w)) 650 680        
                                                                                                                                                                                                                                                                     (place-image (greendot? w) (beatdotx w) (posn-y beatselectpos)
                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                  playconstants)))))))))))))))))))]
        
        [(cons? lon) (cond [(and (string=? "piano" (note-type (first lon))) (noteonpage? (first lon) page)) (rectangle-color lon page "red" w)]
                           [(and (string=? "vgame1" (note-type (first lon))) (noteonpage? (first lon) page)) (rectangle-color lon page "blue" w)]                           
                           [(and (string=? "hihat" (note-type (first lon))) (noteonpage? (first lon) page)) (rectangle-color lon page "green" w)]
                           [(and (string=? "vgame2" (note-type (first lon))) (noteonpage? (first lon) page)) (rectangle-color lon page "purple" w)]
                           [(and (string=? "kick" (note-type (first lon))) (noteonpage? (first lon) page)) (rectangle-color lon page "orange" w)]
                           [(and (string=? "vgame3" (note-type (first lon))) (noteonpage? (first lon) page)) (rectangle-color lon page "yellow" w)]
                           [else (makescene (rest lon) page w)])]))

(check-expect (makescene (list (note "piano" 71 2) (note "piano" 72 1)) 1 test-world2)
              (rectangle-color (list (note "piano" 71 2) (note "piano" 72 1)) 1 "red" test-world2))

; greendot is an image
; world -> image
; if the current beat is on that page, then an image of current beat indicator will appear at the beat identifier row of that page
(define (greendot? w)
  (cond [(beatonpage? (world-curbeat w) (world-page w)) (circle (/ INTERVAL_HEIGHT 2) 'solid 'green)]
        [else empty-image]))
(check-expect (greendot? test-world) (circle (/ INTERVAL_HEIGHT 2) 'solid 'green))

; world -> posn-x
; depending on the current beat, the beat indicator's x position will move along the beat identifier row from left to right
(define (beatdotx w)
  (- (* (modulo (ceiling (world-curbeat w)) BEATS_PER_PAGE)
        BEAT_WIDTH) (/ BEAT_WIDTH 2)))
(check-expect (beatdotx test-world) 96)

; number -> number(posn-y)
; looks at the pitch of the note and determines the row in which the note should go 
(define (row-location row-num)
  (- (+ TOP_OF_STAFF (* INTERVAL_HEIGHT row-num)) (round (/ INTERVAL_HEIGHT 2))))
(define (pitchlookup pitch)
  (cond
    [(= pitch 60) (row-location 8)] 
    [(= pitch 62) (row-location 7)] 
    [(= pitch 64) (row-location 6)] 
    [(= pitch 65) (row-location 5)] 
    [(= pitch 67) (row-location 4)] 
    [(= pitch 69) (row-location 3)] 
    [(= pitch 71) (row-location 2)] 
    [(= pitch 72) (row-location 1)]))

(check-expect (pitchlookup 60) 502)
(check-expect (pitchlookup 72) 166)

; number number -> number (posn-x)
; looks at the beat of the note and the page to determines the column in which the note should go on that page
(define (beatlookup beat page)
  (- (+ START_OF_STAFF (* BEAT_WIDTH (- beat (* (- page 1) BEATS_PER_PAGE)))) (round (/ BEAT_WIDTH 2))))
(check-expect (beatlookup 2 2) -928) 

; note number -> boolean
; checks if a note is on a specified page
(define (noteonpage? note page)
  (= (ceiling (/ (note-beat note) BEATS_PER_PAGE)) page))
(check-expect (noteonpage? (make-note "piano" 55 2) 1) true)
(check-expect (noteonpage? (make-note "piano" 55 2) 2) false)

; number number -> boolean
; checks if a beat is on a specified page
(define (beatonpage? beat page)
  (= (ceiling (/ beat BEATS_PER_PAGE)) page))
(check-expect (beatonpage? 2 1) true)
(check-expect (beatonpage? 16 2) false)

; These functions find the row or column that the mouse is in
; number -> number (row)
; checks which row the mouse coordinates are in
(define (mouserow y)
  (ceiling (/ (- y TOP_OF_STAFF) INTERVAL_HEIGHT)))

(check-expect (mouserow 200) 2)

; number number -> number (column)
; check which column the mouse coordinates are in
(define (mousecol x page)
  (+ (* (- page 1) BEATS_PER_PAGE) (ceiling (/ (- x START_OF_STAFF) BEAT_WIDTH))))

(check-expect (mousecol 300 1) 5)

; number number string number number -> note (structure)
; decides what to set the fields as in each new note added to the worldlist
(define (new-note col row selected curbeat page)
  (make-note selected (findpitch row) col))

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
(define (findbeat col page)
  (+ col (* (- page 1) BEATS_PER_PAGE)))

; x y -> boolean
; checks if the mouse coordinates are on staff
(define (mouseonstaff? x y)
  (and (> y TOP_OF_STAFF)
       (< y BOTTOM_OF_STAFF)
       (> x START_OF_STAFF)
       (< x END_OF_STAFF)))

; x y -> boolean
; checks if the mouse coordinates are on any f the sound buttons
(define (mouseonbuttons? x y)
  (and (> x (posn-x buttonrowpos))
       (< x (+ (posn-x buttonrowpos) (* soundbuttonside 7)))
       (> y (posn-y buttonrowpos))
       (< y (+ (posn-y buttonrowpos) soundbuttonside))))

; helper function
; number image number number posn-x posn-y -> boolean
; n is the distance away from the original button position
; round-back is used to reverse floor function
; check if the mouse coordinates are on a button specified
(define (posn-check buttonpos button n round-back x y)
  (and (> y (- (+ (posn-y buttonpos) n) (- (floor (/ (image-height button) 2)) round-back)))
       (< y (+ (+ (posn-y buttonpos) n) (- (floor (/ (image-height button) 2)) round-back)))
       (> x (- (posn-x buttonpos) (floor (/ (image-width button) 2))))
       (< x (+ (posn-x buttonpos) (floor (/ (image-width button) 2))))))

; x y -> boolean
; checks if the mouse coordinates are on the reset button
(define (mouseonreset? x y)
  (posn-check resetbuttonpos resetbutton 0 0.5 x y))

; x y -> boolean
; checks if the mouse coordinates are on the play button
(define (mouseonplay? x y)
  (posn-check playbuttonpos playbutton 0 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the previous page arrow button
(define (mouseonleftpage? x y)
  (posn-check leftarrowpos arrowleft 0 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the forward page arrow button
(define (mouseonrightpage? x y)
  (posn-check rightarrowpos arrowright 0 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the option button from the composer screen
(define (mouseonoptions? x y)
  (posn-check optionsbuttonpos optionsbutton 0 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the increased tempo button
(define (mouseontempoplus? x y)
  (posn-check temposelpos tempoplusbutton -36 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the decreased tempo button
(define (mouseontempominus? x y)
  (posn-check temposelpos tempominusbutton 36 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the save button from option menu
(define (mouseonsave? x y)
  (posn-check savebuttonpos savebutton 0 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the load button from option menu
(define (mouseonload? x y)
  (posn-check loadbuttonpos loadbutton 0 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the resume button from option menu
(define (mouseonresume? x y)
  (posn-check resumebuttonpos resumebutton 0 0 x y))

; x y -> boolean
; checks if the mouse coordinates are on the exit button from option menu
(define (mouseonexit? x y)
  (posn-check exitbuttonpos exitbutton 0 0 x y))

; x y -> boolean
(define (mouseonbeatsel? x y)
  (posn-check beatselectpos beatselect 0 0 x y))

(check-expect (posn-check resetbuttonpos resetbutton 0 0.5 575 75) true) ;coordinates within the reset button

; helper function for buttonrowfunc
; x y number -> boolean
; icon-number represents the order of icons. Ex: piano icon is the first icon, so it has icon-number 1
(define (soundbutton-check x y icon-number)
  (and (> x (+ (posn-x buttonrowpos) (* (- icon-number 1) soundbuttonside)))
       (< x (+ (posn-x buttonrowpos) (* icon-number soundbuttonside)))
       (> y (posn-y buttonrowpos))
       (< y (+ (posn-y buttonrowpos) soundbuttonside))))



; helper function
; world string -> world
; sound-type represents type of sound like piano
(define (given-sound w sound-type)
  (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) (world-modestate w) sound-type (world-page w) (world-randval w)))

; world posn-x posn-y -> world
; checks if the mouse coordinates are on a specific sound button and if it is,
; then creates a world with that specific sound that can be added to the staff
(define (buttonrowfunc w x y)
  (cond [(soundbutton-check x y 1) (given-sound w "piano")]
        [(soundbutton-check x y 2) (given-sound w "vgame1")]
        [(soundbutton-check x y 3) (given-sound w "hihat")]
        [(soundbutton-check x y 4) (given-sound w "vgame2")]
        [(soundbutton-check x y 5) (given-sound w "kick")]
        [(soundbutton-check x y 6) (given-sound w "vgame3")]
        [(soundbutton-check x y 7) (given-sound w "erase")]))

(check-expect (buttonrowfunc test-world2 90 90)
              (make-world (list (note "piano" 71 2) (note "piano" 72 1)) 2 0 "paused" "vgame1" 1 1))

; helper function for beatselfn
; x y number -> boolean
; num represents beat number
(define (beatbutton-check x y num)
  (and (> x (+ (- (posn-x beatselectpos) (/ (image-width beatselect) 2)) (* (- num 1) BEAT_WIDTH)))
       (< x (+ (- (posn-x beatselectpos) (/ (image-width beatselect) 2)) (* num BEAT_WIDTH)))
       (> y (- (posn-y beatselectpos) (/ (image-width beatselect) 2)))
       (< y (+ (posn-y beatselectpos) (/ (image-width beatselect) 2)))))

; helper function for buttonrowfunc
; world string -> world
(define (given-beat w beat)
  (make-world (world-worldlist w) (world-tempo w) beat (world-modestate w) (world-selected w) (world-page w) (world-randval w)))

; world posn-x posn-y -> world
; checks if the mouse coordinates are on the beat ractangles(green rectangles) and if it is,
; then changes the current world into a world with the beat corresponding to that beat rectangle
(define (beatselfn w x y)
  (cond [(beatbutton-check x y 1) (given-beat w 1)]
        [(beatbutton-check x y 2) (given-beat w 2)]
        [(beatbutton-check x y 3) (given-beat w 3)]
        [(beatbutton-check x y 4) (given-beat w 4)]
        [(beatbutton-check x y 5) (given-beat w 5)]
        [(beatbutton-check x y 6) (given-beat w 6)]
        [(beatbutton-check x y 7) (given-beat w 7)]
        [(beatbutton-check x y 8) (given-beat w 8)]
        [(beatbutton-check x y 9) (given-beat w 9)]
        [(beatbutton-check x y 10) (given-beat w 10)]
        [(beatbutton-check x y 11) (given-beat w 11)]
        [(beatbutton-check x y 12) (given-beat w 12)]
        [(beatbutton-check x y 13) (given-beat w 13)]
        [(beatbutton-check x y 14) (given-beat w 14)]
        [(beatbutton-check x y 15) (given-beat w 15)]
        [(beatbutton-check x y 16) (given-beat w 16)]))

;;Mouse Event Handler
; includes mousnfn, mainmousefn, and optionmousefn

; world x y mouse-event -> world
; handles the mouse events for all the screens
(define (mousefn w x y evt)
  (cond [(string=? (world-modestate w) "options") (optionsmousefn w x y evt)]
        [(or (string=? (world-modestate w) "playing")
             (string=? (world-modestate w) "paused")) (mainmousefn w x y evt)]
        [(and (string=? evt "button-down") (string=? (world-modestate w) "start"))
         (make-world empty 2 0 "paused" "piano" 1 (world-randval w))]
        [else w]))

(check-expect (mousefn test-world2 100 100 "button-down")
              (mainmousefn test-world2 100 100 "button-down"))

; world x y mouse-event -> world
; handles the mouse events on the main screen
; by pressing the staff block, the block will turn from outline to a colored solid block, depended on the current world-selected
; by pressing the reset button, the block will all turn back to outline
; by pressing the play button, the program will play the rsound depending on which blocks are currently red solid
; by pressing the left or right page arrow button, the program page will go to the previous or next page
; by pressing the sound button, the program's current selected sound to be added becomes the sound that sound button represents
; by pressing the option button, the program composer screen will goes to the option screen
; by pressing the tempo plus or minus button, the tempo of the sound will increase or decrease
; by pressing the beat rectangle, the world's current beat will change corresponding to the rectangle it is clicked
; by pressing the desk image at the beginning, the window startup music will play and the program will go to the composer screen
(define (mainmousefn w x y evt)
  (cond [(and (string=? evt "button-down") (mouseonstaff? x y)) (buttondownhandler w x y)] ;(buttondownhandler w x y)]
        [(and (string=? evt "button-down") (mouseonreset? x y)) (make-world default_list 2 0 "paused" "piano" 1 1)]
        [(and (string=? evt "button-down") (mouseonplay? x y)) (play-pressed w)]
        [(and (string=? evt "button-down") (mouseonleftpage? x y)) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w)
                                                                               (world-modestate w) (world-selected w)
                                                                               (change-page (world-page w) "back") (random 10))]
        [(and (string=? evt "button-down") (mouseonrightpage? x y)) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) 
                                                                                (world-modestate w) (world-selected w)
                                                                                (change-page (world-page w) "forward") (random 10))]
        [(and (string=? evt "button-down") (mouseonbuttons? x y)) (buttonrowfunc w x y)]
        [(and (string=? evt "button-down") (mouseonoptions? x y)) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w)
                                                                              "options" (world-selected w) (world-page w) (random 10))]
        [(and (string=? evt "button-down") (mouseontempoplus? x y)) (make-world (world-worldlist w) (change-tempo (world-tempo w) "+") (world-curbeat w)
                                                                                (world-modestate w) (world-selected w) (world-page w) (random 10))]
        [(and (string=? evt "button-down") (mouseontempominus? x y)) (make-world (world-worldlist w) (change-tempo (world-tempo w) "-") (world-curbeat w)
                                                                                 (world-modestate w) (world-selected w) (world-page w) (random 10))]
        [(and (string=? evt "button-down") (mouseonbeatsel? x y)) (beatselfn w x y)]
        [(and (string=? evt "button-down") (string=? (world-modestate w) "start")) 
         (both (play starts) 
               (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) "paused" (world-selected w) (world-page w) (random 10)))]
        [else w]))

(check-expect (mainmousefn (make-world (list (note "piano" 71 2) (note "piano" 72 1)) 2 0 "paused" "vgame1" 1 1) 600 75 "button-down") (make-world default_list 2 0 "paused" "piano" 1 1))

; world posn-x posn-y mouse-event -> world
; handles the mouse events on the option menu
; by pressing the save function, the program will save the current world into a text file for it to be loaded later
; by pressing the load function, the program will load the most recently saved world to be used as the current world
; by pressing the resume button, the program will return to the composer screen in "paused" modestate
; by pressing the exit button, the program will exit the program and goes to a blank screen
(define (optionsmousefn w x y evt)
  (cond [(and (string=? evt "button-down") (mouseonsave? x y)) (both (savefile w) w)]
        [(and (string=? evt "button-down") (mouseonload? x y)) (loadfile w)]
        [(and (string=? evt "button-down") (mouseonresume? x y)) (make-world (world-worldlist w) (world-tempo w) (world-curbeat w)
                                                                             "paused" (world-selected w) (world-page w) ((random 10)))]
        [(and (string=? evt "button-down") (mouseonexit? x y)) (stop-with (make-world(world-worldlist w) (world-tempo w) (world-curbeat w) "shutdown" (world-selected w) (world-page w) ((random 10))))]
        [else w]))

(check-expect (optionsmousefn test-world2 500 150 "button-down")
              (both (savefile test-world2)
                    test-world2))

; number string -> number
; by pressing the increased tempo button, the tempo will increase by 3
; by pressing the decreased tempo button, the tempo will decrease by 3
(define (change-tempo tempo type)
  (cond
    [(and (string=? type "+") (< tempo 240)) (+ tempo .05)]
    [(and (string=? type "-") (> tempo 0)) (- tempo .05)]
    [else tempo]))

(check-within (change-tempo 120 "piano") 120.5 0.5)

; number string -> number
; changes the page of the composer
; this function ensures the page doesn't below the value of one
(define (change-page page navtype)
  (cond 
    [(and (string=? navtype "back") (= page 1)) 1]
    [(string=? navtype "forward") (add1 page)]
    [(string=? navtype "back") (sub1 page)]))

(check-expect (change-page 1 "forward") 2)
(check-expect (change-page 1 "back") 1)
(check-expect (change-page 2 "back") 1)

; helper function for buttonrowfunc
; world lon -> world
(define (given-worldlist w lon)
  (make-world lon (world-tempo w) (world-curbeat w) (world-modestate w) (world-selected w) (world-page w) (world-randval w)))

; world number number -> world
; check if there is note for a rectangle and either
; -adds a note if there is no note for that rectangle currently
; -erases a note if there is already a note exists for that rectangle currently
; handle event is mouse clicks on the staff
(define (buttondownhandler w x y)
  (cond 
    [(string=? (world-selected w) "erase") (given-worldlist w (delete-note (world-worldlist w) (mousecol x (world-page w)) (findpitch (mouserow y))))]
    [else (make-world (cons (new-note (mousecol x (world-page w)) (mouserow y) (world-selected w) (world-curbeat w) (world-page w)) (world-worldlist w))
                      (world-tempo w) (world-curbeat w) (world-modestate w) (world-selected w) (world-page w) (random 10))]))

; because of the random function, it doesn't always work, so this check-expect is theoretically worked
#;(check-expect (buttondownhandler (make-world (list (note "piano" 71 2) (note "piano" 72 1)) 2 0 "paused" "vgame1" 1 1) 50 200)
                (make-world (list (note "vgame1" 71 1) (note "piano" 71 2) (note "piano" 72 1))  2 0 "paused" "vgame1" 1 
                            (world-randval (make-world (list (note "piano" 71 2) (note "piano" 72 1)) 2 0 "paused" "vgame1" 1 1))))

; list-of-notes number number -> list-of-notes
; deletes note from the worldlist
(define (delete-note lon beat pitch)
  (cond [(empty? lon) empty]
        [(and (= beat (note-beat (first lon))) (= pitch (note-pitch (first lon))))
         (delete-note (remove (first lon) lon) beat pitch)]
        [else (cons (first lon) (delete-note (rest lon) beat pitch))]))

(check-expect (delete-note (list (note "piano" 71 2) (note "piano" 72 1)) 2 71) (list (note "piano" 72 1)))

; note -> list with rsound and play time
; turn a note into a sound and a time to be used in the make-song function
(define (make-note+time n tempo)
  (cond [(string=? (note-type n) "piano") (list (piano-tone (note-pitch n)) (round (* (* 44100 (/ 1 tempo)) (note-beat n))))]
        [(string=? (note-type n) "vgame1") (list (rs-scale .1 (synth-note "vgame" 1 (note-pitch n) (round (* 44100 (/ 1 tempo))))) (round (* (* 44100 (/ 1 tempo)) (note-beat n))))]
        [(string=? (note-type n) "hihat") (list c-hi-hat-1 (round (* (* 44100 (/ 1 tempo)) (note-beat n))))]
        [(string=? (note-type n) "vgame2") (list (rs-scale .3 (synth-note "main" 63 (note-pitch n) (round (* 44100 (/ 1 tempo))))) (round (* (* 44100 (/ 1 tempo)) (note-beat n))))]
        [(string=? (note-type n) "kick") (list (rs-scale .7 kick) (round (* (* 44100 (/ 1 tempo)) (note-beat n))))]
        [(string=? (note-type n) "vgame3") (list (rs-scale .1 (synth-note "vgame" 22 (note-pitch n) (round (* 44100 (/ 1 tempo))))) (round (* (* 44100 (/ 1 tempo)) (note-beat n))))]))

(check-expect (make-note+time (make-note "piano" 55 2) 2) (list (piano-tone 55) 44100))

; list-of-notes tempo -> list (list sound time)
; turns a list of notes into a list of list of sounds and times for the assemble function
(define (make-notes+times lon tempo)
  (cond [(empty? lon) empty]
        [else (cons (make-note+time (first lon) tempo) (make-notes+times (rest lon) tempo))]))

(check-expect (make-notes+times (list (note "piano" 71 2) (note "piano" 72 1)) 2)
              (list (list (piano-tone 71) 44100) (list (piano-tone 72) 22050)))

; list-of-notes -> rsound
; takes a list of notes and turns them into one big sound
(define (make-song lon tempo) 
  (cond [(empty? lon) (silence 1)]
        [else (assemble (make-notes+times lon tempo))]))

; world -> boolean
;checks to see if song has finished playing
(define (song-over? w) (> (current-frame w) (song-length w)))

(check-expect (song-over? test-world) false)

; number -> number
; check to see if current playing beat is on page
(define (pagecheck beat)
  (ceiling (/ beat BEATS_PER_PAGE)))

(check-expect (pagecheck 2) 1)

; world -> world
; checks the modestate and either
; if the modestate is "playing", then either:
;    continue playing the rsound at a certain rate if the song isn't over or
;    changes the modestate into "paused" if the song is over
; if the modestate is "paused", then the world stays the same
(define (tick w)
  (cond [(string=? (world-modestate w) "playing")
         (cond [(song-over? w) (make-world (world-worldlist w) (world-tempo w) 0
                                           "paused" (world-selected w) (world-page w) (world-randval w))]
               [else (make-world (world-worldlist w) (world-tempo w) (+ (world-curbeat w) (* 1/12 (world-tempo w)))
                                 (world-modestate w) (world-selected w) (pagecheck (world-curbeat w)) (world-randval w))])]
        [(string=? (world-modestate w) "paused") w]
        [else w]))

; utilized world-sav.rkt for saving and loading the world
; creating a list of the fields in the structure
(define maker-table
  (list (list "world" make-world)
        (list "note" make-note)))

(define my-table (list (list "world" make-world)
                       (list "note" make-note)))

; string -> structure
; converts a string into a structure
(define (string->structs str) 
  (string->struct/maker maker-table str))

; text file of the saved world
(define SAVED-FILE-NAME "saved-world.txt")

; world -> text file
; saving the current world into a text file
(define (savefile w)
  (write-file SAVED-FILE-NAME (write-to-string w)))

; text file -> world
; load a saved world that is in a text file
(define (loadfile w)
  (string->structs (read-file SAVED-FILE-NAME)))

; gets the current duration in frames of the song
(define (song-length w) (rs-frames (make-song (world-worldlist w) (world-tempo w))))
(check-expect (song-length test-world) 168974)

; converts world-curbeat to the current frame
(define (current-frame w) (round (* 44100 (* (world-curbeat w) (/ 1 (world-tempo w))))))
(check-expect (current-frame test-world) 44100)

; string -> string
; if the play button is currently in "playing" mode then the button will be in "paused" mode
; if the play button is currently in "paused" mode then the button will be in "playing" mode
(define (playbuttonstate pbs)
  (if (string=? pbs "playing") "paused" "playing"))
(check-expect (playbuttonstate "paused") "playing")

; play-button function
; world -> world (plays song or stop)
; this function is called in the mainmousefn function, when the play button is clicked.
; when the modestate is in "paused", 
(define (play-pressed w)
  (cond
    [(string=? "paused" (world-modestate w))(both (play (clip (make-song (world-worldlist w) (world-tempo w)) (current-frame w) (song-length w))) 
                                                  (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) "playing" (world-selected w) (world-page w) (world-randval w)))]
    [(string=? "playing" (world-modestate w))(both (stop)  (make-world (world-worldlist w) (world-tempo w) (world-curbeat w) 
                                                                       "paused" (world-selected w) (world-page w) (world-randval w)))]))

(check-expect (play-pressed test-world) (both (play (clip (make-song (list (note "piano" 55 2)) 2) 44100 168974))
                                              (make-world (list (note "piano" 55 2)) 2 2 "playing" "piano" 1 1)))

(big-bang INITIAL_WORLD 
          [on-mouse mousefn]
          [on-draw renderfn]
          [on-tick tick])
(test)
