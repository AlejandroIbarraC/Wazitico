#lang racket/gui
(require 2htdp/image)

; Main menu Screen
(define menuScreen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

; Load images
(define logoPic (make-object bitmap% "assets/logo.png"))

; Draws menu elements 
(define (drawMenu canvas dc)
  (send dc set-scale 3 3)
  (send dc set-text-foreground "blue")
  (send dc draw-text "Don't Panic!" 125 125)
  (send dc draw-bitmap logoPic -100 0))
  

; Menu canvas
(define menuCanvas (new canvas% [parent menuScreen]
                                [paint-callback drawMenu]))

; Make a static text message in the frame
(define msg (new message% [parent menuScreen]
                          [label "No events so far..."]))

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
(new button% [parent menuScreen]
             [label "Select City"]
             [callback (lambda (button event)
                         (toCitySelectScreen))])





; City select screen
(define citySelectScreen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

; Changes screen to main menu
(define (toMenu)
  (send menuScreen show #t)
  (send citySelectScreen show #f))

; Changes screen to city 1
(define (toCity1)
  (send citySelectScreen show #f)
  (send city1Screen show #t))

; Back to menu button
(new button% [parent citySelectScreen]
             [label "Back"]
             [callback (lambda (button event)
                         (toMenu))])





; City 1 screen
(define city1Screen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

; Back to menu button
(new button% [parent citySelectScreen]
             [label "Arcadia Bay"]
             [callback (lambda (button event)
                         (toCity1))])




(showMenu #t)
