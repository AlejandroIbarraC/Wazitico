#lang racket/gui
(require 2htdp/image)

;----USEFUL METHODS----;

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
(define customcity_map (make-object bitmap% "assets/maps/customcity_map.png"))
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
  (send customCityScreen show #t))


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
                               [alignment '(right center)]))

; Draws city 1 elements in canvas
(define (drawCity1 canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale arcadiabay_map 0.4) 0 0))

; City 1 canvas and drawing context
(define city1Canvas (new canvas% [parent city1Panel]
                                 [paint-callback drawCity1]))

(define city1dc (send city1Canvas get-dc))

; Calculate Button
(new button% [parent city1Panel]
             [label "Calculate"]
             [callback (lambda (button event)
                         (drawLines))])

; Back to menu button
(new button% [parent city1Panel]
             [label "Back"]
             [callback (lambda (button event)
                         (toCitySelectScreen_fromCity1))])

; Draws lines
(define (drawLines)
  (send city1dc draw-line 0 0 100 100))

; Changes screen to city select
(define (toCitySelectScreen_fromCity1)
  (send city1Screen show #f)
  (send citySelectScreen show #t))

;-------------------------Custom City-------------------------;

;----VARIABLES----;

(define nodeList '())
(define connectionList '())
(define testRoute '(("1" "2" "3") ("1" "4" "3")))

; Custom City screen and panel
(define customCityScreen (new frame% [label "Wazecheme"]
                                     [width 1000]
                                     [height 800]
                                     [style '(no-resize-border)]))

(define customCityPanel (new panel% [parent customCityScreen]
                                    [border 0]
                                    [spacing 0]
                                    [alignment '(center center)]))

; Draw initial Custom City elements
(define (drawCustomCity canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale customcity_map 0.25) 0 0))


; Custom City canvas and drawing context
(define customCityCanvas (new canvas% [parent customCityPanel]
                                      [paint-callback drawCustomCity]))

(define customCityDC (send customCityCanvas get-dc))


;----METHODS----;


; Check if node number is repeated
(define (checkRepeatedNodes n)
  (checkRepeatedNodes_aux n nodeList)
  (send addNode_entry set-value ""))

(define (checkRepeatedNodes_aux n list)
  (cond ((equal? n "") #f)
        ((null? list) (drawNode n))
        (else (cond ((equal? (car (car list)) n) #f)
                    (else (checkRepeatedNodes_aux n (cdr list)))))))

; Draws all connections in list
(define (drawAllConnections list)
  (cond ((null? list) #f)
        (else (let* ([currentConnection (car list)]
                     [currentN1 (car currentConnection)]
                     [currentN2 (car (cdr currentConnection))]
                     [currentWeight (car (cdr (cdr currentConnection)))]
                     [currentIsBidirectional (car (cdr (cdr (cdr currentConnection))))])
               (redrawConnection currentN1 currentN2 currentWeight currentIsBidirectional)
               (drawAllConnections (cdr list))))))

; Draws all nodes in list
(define (drawAllNodes list)
  (cond ((null? list) #f)
        (else (let* ([currentNode (car (car list))]
                     [currentPoint (getPoint currentNode)]
                     [currentX (send currentPoint get-x)]
                     [currentY (send currentPoint get-y)])
              (drawNodeInPos currentNode currentX currentY)
              (drawAllNodes (cdr list))))))

; Draws connection between two graphical nodes (CONNECT NODES IN GRAPH)
(define (drawConnection n1 n2 weight isBidirectional)
  (let* ([p1 (getPoint n1)]
         [p2 (getPoint n2)]
         [p1x (send p1 get-x)]
         [p1y (send p1 get-y)]
         [p2x (send p2 get-x)]
         [p2y (send p2 get-y)]
         [middlePoint (getMiddlePoint p1x p1y p2x p2y)]
         [midX (send middlePoint get-x)]
         [midY (send middlePoint get-y)])
    (set! connectionList (append connectionList (list (list n1 n2 weight isBidirectional))))
    (send customCityDC set-pen blue-pen)
    (send customCityDC draw-line (+ p1x 5) (+ p1y 5) (+ p2x 5) (+ p2y 5))
    (send customCityDC set-pen black-pen)
    (send customCityDC draw-text weight (- midX 10) (- midY 10))))

; Draws node in UI (ADD NODE TO GRAPH)
(define (drawNode n)
  (let* ([x (random 470)]
         [y (random 280)]
         [point (make-object point% x y)])
  (send customCityDC set-brush gold-brush)
  (send customCityDC set-pen gold-pen)
  (send customCityDC draw-ellipse x y 10 10)
  (send customCityDC draw-text n x (- y 15))
  (set! nodeList (append nodeList (list (list n point))))))

; Draws node in UI in specific position
(define (drawNodeInPos n x y)
  (let* ([point (make-object point% x y)])
  (send customCityDC set-brush gold-brush)
  (send customCityDC set-pen gold-pen)
  (send customCityDC draw-ellipse x y 10 10)
  (send customCityDC draw-text n x (- y 15))))

; Draws single route
(define (drawRoute route isShortest)
  (let* ([n1 (car route)]
         [nRoute (cdr route)])
  (cond ((null? nRoute) #f)
        (else (let* ([n2 (car nRoute)])
              (drawSimpleConnection n1 n2 isShortest)
              (drawRoute (cdr route) isShortest))))))

; Draws all found routes
(define (drawRoutes routeList isShortest)
  (cond ((null? routeList) #f)
        (else (drawRoute (car routeList) isShortest)
              (drawRoutes (cdr routeList) #f))))

; Draws connection between two nodes without weight or bidirectionality
; Used to draw found routes
(define (drawSimpleConnection n1 n2 isShortest)
  (let* ([p1 (getPoint n1)]
         [p2 (getPoint n2)]
         [p1x (send p1 get-x)]
         [p1y (send p1 get-y)]
         [p2x (send p2 get-x)]
         [p2y (send p2 get-y)]) 
  (send customCityDC set-pen green-pen)
  (cond ((equal? isShortest #t) (send customCityDC set-pen gold-pen)))
  (send customCityDC draw-line (+ p1x 5) (+ p1y 5) (+ p2x 5) (+ p2y 5))
  (send customCityDC set-pen black-pen)))

; Gets point% object of middle point
(define (getMiddlePoint p1x p1y p2x p2y)
  (let* ([resultX (/ (+ p1x p2x) 2)]
         [resultY (/ (+ p1y p2y) 2)])
  (make-object point% resultX resultY)))

; Gets point% object from node name
(define (getPoint n)
  (getPoint_aux n nodeList))

(define (getPoint_aux n list)
  (cond ((null? list) #f)
        ((equal? (car (car list)) n) (car (cdr (car list))))
        (else (getPoint_aux n (cdr list)))))

; Redraws connection between two graphical nodes
(define (redrawConnection n1 n2 weight isBidirectional)
  (let* ([p1 (getPoint n1)]
         [p2 (getPoint n2)]
         [p1x (send p1 get-x)]
         [p1y (send p1 get-y)]
         [p2x (send p2 get-x)]
         [p2y (send p2 get-y)]
         [middlePoint (getMiddlePoint p1x p1y p2x p2y)]
         [midX (send middlePoint get-x)]
         [midY (send middlePoint get-y)])
  (send customCityDC set-pen blue-pen)
  (send customCityDC draw-line (+ p1x 5) (+ p1y 5) (+ p2x 5) (+ p2y 5))
  (send customCityDC set-pen black-pen)
  (send customCityDC draw-text weight (- midX 10) (- midY 10))))

; Redraws graph
(define (redrawGraph)
  (send customCityDC draw-bitmap (bitmap-scale customcity_map 0.25) 0 0)
  (drawAllNodes nodeList)
  (drawAllConnections connectionList))

;----UI ELEMENTS----;

(define addNode_entry (new text-field%
                      (label "Node number")
                      (parent customCityScreen)
                      (init-value "")))

(new button% [parent customCityScreen]
             [label "Add Node"]
             [callback (lambda (button event)
                       (checkRepeatedNodes (send addNode_entry get-value)))])

(define connectOrigin_entry (new text-field%
                            (label "Origin")
                            (parent customCityScreen)
                            (init-value "")))

(define connectDestination_entry (new text-field%
                                 (label "Destination")
                                 (parent customCityScreen)
                                 (init-value "")))

(define connectWeight_entry (new text-field%
                            (label "Weight/Distance")
                            (parent customCityScreen)
                            (init-value "")))

(define isBidirectional (new check-box%
                        (parent customCityScreen)
                        (label "Bidirectional?")
                        (value #f)))

(new button% [parent customCityScreen]
             [label "Connect"]
             [callback (lambda (button event)
                       (drawConnection (send connectOrigin_entry get-value)
                                       (send connectDestination_entry get-value)
                                       (send connectWeight_entry get-value)
                                       (send isBidirectional get-value))
                       (send connectOrigin_entry set-value "")
                       (send connectDestination_entry set-value "")
                       (send connectWeight_entry set-value "")
                       (send isBidirectional set-value #f)
                       (drawAllNodes nodeList))])

(new button% [parent customCityScreen]
             [label "Find route"]
             [callback (lambda (button event)
                         (redrawGraph)
                         (drawRoutes testRoute #t)
                         (drawAllNodes nodeList))])

;-------------------------General-------------------------;
  
;----VARIABLES----;

(define black-pen
  (new pen%
       [color "black"]
       [width 2]))

(define blue-pen
  (new pen%
       [color "blue"]
       [width 2]))

(define gold-brush
  (new brush%
       [color "gold"]))

(define gold-pen
  (new pen%
       [color "gold"]
       [width 3]))

(define green-pen
  (new pen%
       [color "green"]
       [width 3]))

(define transparent-brush
  (new brush%
       [style 'transparent]))


;----ACTIONS----;

(showMenu #t)
