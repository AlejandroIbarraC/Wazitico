#lang racket/gui

(define frame (new frame% [label "Wazecheme"]
                   [width 800]
                   [height 600]
                   [style '(no-resize-border)]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])
 
; Show the frame by calling its show method
(send frame show #t)

