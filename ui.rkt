#lang racket/gui
(require 2htdp/image)

;----METHODS----;

;Blank bitmap for resize
(define bitmap-blank
  (lambda [[w 0] [h #false] #:backing-scale [backing-scale 2.0]]
    (define width  (max 1 (exact-ceiling w)))
    (define height (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale backing-scale)))

;Resize bitmap
(define bitmap-scale
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (let ([w (max 1 (exact-ceiling (* (send bmp get-width) scale-x)))]
                       [h (max 1 (exact-ceiling (* (send bmp get-height) scale-y)))])
                   (define dc (make-object bitmap-dc% (bitmap-blank w h)))
                   (send dc set-smoothing 'aligned)
                   (send dc set-scale scale-x scale-y)
                   (send dc draw-bitmap bmp 0 0)
                   (or (send dc get-bitmap) (bitmap-blank)))])]))

;-------------------------Main Menu-------------------------;

; Main menu Screen
(define menuScreen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

; Load images
(define arcadiabayButtonIcon (make-object bitmap% "assets/arcadiabay_button.png"))
(define arcadiabay_map (make-object bitmap% "assets/maps/arcadiabay_map.png"))
(define background (make-object bitmap% "assets/background.png"))
(define hyruleButtonIcon (make-object bitmap% "assets/hyrule_button.png"))
(define hyrule_map (make-object bitmap% "assets/maps/hyrule_map.png"))
(define logo_namePic (make-object bitmap% "assets/logo_name.png"))
(define logoPic (make-object bitmap% "assets/logo.png"))
(define namePic (make-object bitmap% "assets/name.png"))
(define selectBackground (make-object bitmap% "assets/background_citySelect.png"))
(define startButtonIcon (make-object bitmap% "assets/start_button.png"))

; Menu panel
(define menuPanel (new panel% [parent menuScreen]
                             [border 0]
                             [spacing 0]
                             [alignment '(center center)]))

(define menuPanel_bottom (new panel% [parent menuScreen]
                             [border 0]
                             [spacing 0]
                             [alignment '(center center)]))

; Draws menu elements in canvas
(define (drawMenu canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale background 0.25) 0 0))

(define (drawMenu_bottom canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale background 0.25) 0 -181))

; Menu canvas
(define menuCanvas (new canvas% [parent menuPanel]
                                [paint-callback drawMenu]))

(define menuCanvas_bottom (new canvas% [parent menuPanel_bottom]
                                [paint-callback drawMenu_bottom]))

; Control menu frame show with boolean value
(define (showMenu bool)
  (cond ((equal? bool #t)
         (send menuScreen show #t))
        (else
         (send menuScreen show #f))))

; Changes screen to city selection
(define (toCitySelectScreen)
  (send menuScreen show #f)
  (send citySelectScreen show #t))

; Change screen to city selection button
(new button% [parent menuPanel_bottom]
             [label (bitmap-scale startButtonIcon 0.6)]
             [callback (lambda (button event)
                         (toCitySelectScreen))])

;-------------------------City Select-------------------------;

; City select screen
(define citySelectScreen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

; City select panels
(define selectPanel_left (new panel% [parent citySelectScreen]
                             [border 0]
                             [spacing 0]
                             [alignment '(center center)]))

(define selectPanel_right (new panel% [parent citySelectScreen]
                             [border 0]
                             [spacing 0]
                             [alignment '(center center)]))

; Draws menu elements in canvas
(define (drawSelect_left canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale selectBackground 0.25) 0 0))

(define (drawSelect_right canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale selectBackground 0.25) 0 -197))

; Menu canvas
(define selectCanvas_left (new canvas% [parent selectPanel_left]
                                [paint-callback drawSelect_left]))

(define selectCanvas_right (new canvas% [parent selectPanel_right]
                                [paint-callback drawSelect_right]))

; Changes screen to city 1
(define (toCity1)
  (send citySelectScreen show #f)
  (send city1Screen show #t))

; Changes screen to city 2
(define (toCity2)
  (send citySelectScreen show #f)
  (send city2Screen show #t))


; To city 1 button
(new button% [parent selectPanel_left]
             [label (bitmap-scale arcadiabayButtonIcon 0.6)]
             [callback (lambda (button event)
                         (toCity1))])

; To city 2 button
(new button% [parent selectPanel_right]
             [label (bitmap-scale hyruleButtonIcon 0.6)]
             [callback (lambda (button event)
                         (toCity2))])

;-------------------------Arcadia Bay-------------------------;

; City 1 screen
(define city1Screen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

; City 1 panel
(define city1Panel (new panel% [parent city1Screen]
                             [border 0]
                             [spacing 0]
                             [alignment '(center center)]))

; Draws city 1 elements in canvas
(define (drawCity1 canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale arcadiabay_map 0.5) 0 0))

; City 1 canvas
(define city1Canvas (new canvas% [parent city1Panel]
                                [paint-callback drawCity1]))

; Back to menu button
(new button% [parent city1Screen]
             [label "Back"]
             [callback (lambda (button event)
                         (toCitySelectScreen_fromCity1))])

; Changes screen to city select
(define (toCitySelectScreen_fromCity1)
  (send city1Screen show #f)
  (send citySelectScreen show #t))

;-------------------------Hyrule-------------------------;

; City 2 screen
(define city2Screen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

; City 2 panel
(define city2Panel (new panel% [parent city2Screen]
                             [border 0]
                             [spacing 0]
                             [alignment '(center center)]))

; Draws city 2 elements in canvas
(define (drawCity2 canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale hyrule_map 0.45) 0 0))

; City 2 canvas
(define city2Canvas (new canvas% [parent city2Panel]
                                [paint-callback drawCity2]))


; Back to menu button
(new button% [parent city2Screen]
             [label "Back"]
             [callback (lambda (button event)
                         (toCitySelectScreen_fromCity2))])

(define (toCitySelectScreen_fromCity2)
  (send city2Screen show #f)
  (send citySelectScreen show #t))

;-------------------------General-------------------------;
  
;----VARIABLES----;
  
(define transparent-brush
  (new brush%
       [style 'transparent]))

(define gold-pen
  (new pen%
       [color "gold"]
       [width 8]))

;----ACTIONS----;

(showMenu #t)
