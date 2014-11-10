;Including required libraries, and the infamous both function
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)
(define (both a b) b)

;Draw Function
; Draws 8x8 grid with squares filled in marked by the world
; world -> image

; Function for empty/filled button
(define (button filled?)
  (if filled? (overlay (square 73 'solid 'red)
                       (square 75 'solid 'black))
      (square 75 'outline 'black)))

; world -> image
(define (renderfn world)
  (place-image/align (drawfn (world-grid world))
                     0 0
                     "left" "top"
                      (place-image (pause-button (world-playing? world))
                                  800 100
                                  (place-image (circle 10 'solid 'blue)
                                  (coldotx (world-col world)) (+ (* 75 8) 30)
                                  (place-image resetbutton
                                               800 250
                                  (empty-scene 1000 (* 75 9)))))))

(define (coldotx colnum)
  (cond [(= colnum 1) 37]
        [(= colnum 2) (+ 37 75)]
        [(= colnum 3) (+ 37 (* 75 2))]
        [(= colnum 4) (+ 37 (* 75 3))]
        [(= colnum 5) (+ 37 (* 75 4))]
        [(= colnum 6) (+ 37 (* 75 5))]
        [(= colnum 7) (+ 37 (* 75 6))]
        [(= colnum 0) (+ 37 (* 75 7))]))

(define pausebars
  (overlay/offset (rectangle 20 70 'solid 'red)
                  30 0 (rectangle 20 70 'solid 'red)))

(define resetbutton
  (place-image (text "RESET" 16 'blue)
               50 50
               (square 100 'solid 'red)))

; Boolean -> image
(define (pause-button bool)
  (place-image
   (cond [bool (rotate 270 (isosceles-triangle 70 35 'solid 'green))]
         [else pausebars])
   50 50
   (square 100 'solid 'black)))


; grid -> image
(define (drawfn grid) 
  (beside (colrender (grid-c0 grid))
         (colrender (grid-c1 grid))
         (colrender (grid-c2 grid))
         (colrender (grid-c3 grid))
         (colrender (grid-c4 grid))
         (colrender (grid-c5 grid))
         (colrender (grid-c6 grid))
         (colrender (grid-c7 grid))))


; List -> image
(define (colrender collist) 
  (above (button (list-ref collist 0))
          (button (list-ref collist 1))
          (button (list-ref collist 2))
          (button (list-ref collist 3))
          (button (list-ref collist 4))
          (button (list-ref collist 5))
          (button (list-ref collist 6))
          (button (list-ref collist 7))))

(define (s n)(* n 44100))

(define ps (make-pstream))

(define sil (silence 1))

(define tempo 14)

(define play-time (round(s (/ tempo 20))))

(define sound8 (make-tone 261.63 .125 play-time))
(define sound7 (make-tone 293.66 .125 play-time))
(define sound6 (make-tone 329.63 .125 play-time))
(define sound5 (make-tone 349.23 .125 play-time))
(define sound4 (make-tone 392.00 .125 play-time))
(define sound3 (make-tone 440.00 .125 play-time))
(define sound2 (make-tone 493.88 .125 play-time))
(define sound1 (make-tone 523.25 .125 play-time))

;called by tock function, determines which sounds to play when it is time t play a column

(define (sound-col col)
  (rs-overlay* (list
                (if (list-ref col 0) sound1 sil)
                (if (list-ref col 1) sound2 sil)
                (if (list-ref col 2) sound3 sil)
                (if (list-ref col 3) sound4 sil)
                (if (list-ref col 4) sound5 sil)
                (if (list-ref col 5) sound6 sil)
                (if (list-ref col 6) sound7 sil)
                (if (list-ref col 7) sound8 sil))))
;tock function
(define (tock w)(cond
                   [(not (world-playing? w)) w]
                   [(< (world-tick w) (world-tempo w)) (make-world (world-grid w) (world-col w) (+ (world-tick w) 1) (world-tempo w) true)]
                   [(= (world-tick w) (world-tempo w)) (cond
                       [(= (world-col w) 0) (both (pstream-play ps (sound-col (grid-c0 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                       [(= (world-col w) 1) (both (pstream-play ps (sound-col (grid-c1 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                       [(= (world-col w) 2) (both (pstream-play ps (sound-col (grid-c2 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                       [(= (world-col w) 3) (both (pstream-play ps (sound-col (grid-c3 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                       [(= (world-col w) 4) (both (pstream-play ps (sound-col (grid-c4 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                       [(= (world-col w) 5) (both (pstream-play ps (sound-col (grid-c5 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                       [(= (world-col w) 6) (both (pstream-play ps (sound-col (grid-c6 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                       [(= (world-col w) 7) (both (pstream-play ps (sound-col (grid-c7 (world-grid w)))) (make-world (world-grid w) (if (< (world-col w) 7) (+ (world-col w) 1) 0) 0 (world-tempo w) true))]
                   )]
                   ))

(define (lookup x) ;finds column/row event occurs in
  (cond
  [(and (<= x 75) (>= x 0)) 0]
  [(and (<= x 150) (>= x 76)) 1]
  [(and (<= x 225) (>= x 151)) 2]
  [(and (<= x 300) (>= x 226)) 3]
  [(and (<= x 375) (>= x 301)) 4]
  [(and (<= x 450) (>= x 376)) 5]
  [(and (<= x 525) (>= x 451)) 6]
  [(and (<= x 600) (>= x 526)) 7]
  [(and (<= x 850) (>= x 750)) 8]
  [else #f]
  ))

(define (rev-vals x) ;reverses column values between true and false
  (not x))

(define (write-col ls pos)
  (cond
  [(= pos 0) (list (rev-vals (list-ref ls 0)) (list-ref ls 1) (list-ref ls 2) (list-ref ls 3) (list-ref ls 4) (list-ref ls 5) (list-ref ls 6) (list-ref ls 7))]
  [(= pos 1) (list (list-ref ls 0) (rev-vals (list-ref ls 1)) (list-ref ls 2) (list-ref ls 3) (list-ref ls 4) (list-ref ls 5) (list-ref ls 6) (list-ref ls 7))]
  [(= pos 2) (list (list-ref ls 0) (list-ref ls 1) (rev-vals (list-ref ls 2)) (list-ref ls 3) (list-ref ls 4) (list-ref ls 5) (list-ref ls 6) (list-ref ls 7))]
  [(= pos 3) (list (list-ref ls 0) (list-ref ls 1) (list-ref ls 2) (rev-vals (list-ref ls 3)) (list-ref ls 4) (list-ref ls 5) (list-ref ls 6) (list-ref ls 7))]
  [(= pos 4) (list (list-ref ls 0) (list-ref ls 1) (list-ref ls 2) (list-ref ls 3) (rev-vals (list-ref ls 4)) (list-ref ls 5) (list-ref ls 6) (list-ref ls 7))]
  [(= pos 5) (list (list-ref ls 0) (list-ref ls 1) (list-ref ls 2) (list-ref ls 3) (list-ref ls 4) (rev-vals (list-ref ls 5)) (list-ref ls 6) (list-ref ls 7))]
  [(= pos 6) (list (list-ref ls 0) (list-ref ls 1) (list-ref ls 2) (list-ref ls 3) (list-ref ls 4) (list-ref ls 5) (rev-vals (list-ref ls 6)) (list-ref ls 7))]
  [(= pos 7) (list (list-ref ls 0) (list-ref ls 1) (list-ref ls 2) (list-ref ls 3) (list-ref ls 4) (list-ref ls 5) (list-ref ls 6) (rev-vals (list-ref ls 7)))]
  [else #f]))
  
(define (write-world w x y)
  (cond
    [(= (lookup x) 0) (make-world (make-grid (write-col (grid-c0 (world-grid w)) (lookup y)) (grid-c1 (world-grid w)) (grid-c2 (world-grid w)) (grid-c3 (world-grid w)) (grid-c4 (world-grid w)) (grid-c5 (world-grid w)) (grid-c6 (world-grid w)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(= (lookup x) 1) (make-world (make-grid (grid-c0 (world-grid w)) (write-col (grid-c1 (world-grid w)) (lookup y)) (grid-c2 (world-grid w)) (grid-c3 (world-grid w)) (grid-c4 (world-grid w)) (grid-c5 (world-grid w)) (grid-c6 (world-grid w)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(= (lookup x) 2) (make-world (make-grid (grid-c0 (world-grid w)) (grid-c1 (world-grid w)) (write-col (grid-c2 (world-grid w)) (lookup y)) (grid-c3 (world-grid w)) (grid-c4 (world-grid w)) (grid-c5 (world-grid w)) (grid-c6 (world-grid w)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(= (lookup x) 3) (make-world (make-grid (grid-c0 (world-grid w)) (grid-c1 (world-grid w)) (grid-c2 (world-grid w)) (write-col (grid-c3 (world-grid w)) (lookup y)) (grid-c4 (world-grid w)) (grid-c5 (world-grid w)) (grid-c6 (world-grid w)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(= (lookup x) 4) (make-world (make-grid (grid-c0 (world-grid w)) (grid-c1 (world-grid w)) (grid-c2 (world-grid w)) (grid-c3 (world-grid w)) (write-col (grid-c4 (world-grid w)) (lookup y)) (grid-c5 (world-grid w)) (grid-c6 (world-grid w)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(= (lookup x) 5) (make-world (make-grid (grid-c0 (world-grid w)) (grid-c1 (world-grid w)) (grid-c2 (world-grid w)) (grid-c3 (world-grid w)) (grid-c4 (world-grid w)) (write-col (grid-c5 (world-grid w)) (lookup y)) (grid-c6 (world-grid w)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(= (lookup x) 6) (make-world (make-grid (grid-c0 (world-grid w)) (grid-c1 (world-grid w)) (grid-c2 (world-grid w)) (grid-c3 (world-grid w)) (grid-c4 (world-grid w)) (grid-c5 (world-grid w)) (write-col (grid-c6 (world-grid w)) (lookup y)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(= (lookup x) 7) (make-world (make-grid (grid-c0 (world-grid w)) (grid-c1 (world-grid w)) (grid-c2 (world-grid w)) (grid-c3 (world-grid w)) (grid-c4 (world-grid w)) (grid-c5 (world-grid w)) (grid-c6 (world-grid w)) (write-col (grid-c7 (world-grid w)) (lookup y))) (world-col w) (world-tick w) (world-tempo w) (world-playing? w))]
    [(and (= (lookup x) 8) (or (= (lookup y) 0) (= (lookup y) 1))) (make-world (make-grid (grid-c0 (world-grid w)) (grid-c1 (world-grid w)) (grid-c2 (world-grid w)) (grid-c3 (world-grid w)) (grid-c4 (world-grid w)) (grid-c5 (world-grid w)) (grid-c6 (world-grid w)) (grid-c7 (world-grid w))) (world-col w) (world-tick w) (world-tempo w) (not (world-playing? w)))]
    [(not (lookup x)) w]
    ))
(define (mousefn w x y evt) 
  (cond
    [(string=? evt "button-down") (write-world w x y)]
    [else w]
    ))
  
(define-struct world (grid col tick tempo playing?))

(define-struct grid (c0 c1 c2 c3 c4 c5 c6 c7))

(define INITIAL_COLUMN (list false false false false false false false false))
(define INITIAL (make-world (make-grid INITIAL_COLUMN INITIAL_COLUMN INITIAL_COLUMN INITIAL_COLUMN                                 
                                       INITIAL_COLUMN INITIAL_COLUMN INITIAL_COLUMN INITIAL_COLUMN)
                          0 0 tempo true))

(big-bang INITIAL
          [to-draw renderfn]
        [on-mouse mousefn]
          [on-tick tock]
          )
