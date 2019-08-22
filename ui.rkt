#lang racket/gui
(require 2htdp/image racket/draw/arrow
         "logic-src/graph-lib.rkt"
         "logic-src/graph-maker.rkt"
         "logic-src/path-engine.rkt"
         "logic-src/arcadiaBay-graph.rkt")

;-------------------------Startup-------------------------;

;----ACTIONS----;

; Load images
(define addButtonIcon (make-object bitmap% "assets/add_button.png"))
(define arcadiabayButtonIcon (make-object bitmap% "assets/arcadiabay_button.png"))
(define arcadiabay_map (make-object bitmap% "assets/maps/arcadiabay_map.png"))
(define background (make-object bitmap% "assets/background_trash.png"))
(define connectButtonIcon (make-object bitmap% "assets/connect_button.png"))
(define customcityButtonIcon (make-object bitmap% "assets/customcity_button.png"))
(define findRouteButtonIcon (make-object bitmap% "assets/findroute_button.png"))
(define customcity_map (make-object bitmap% "assets/maps/customcity_map.png"))
(define logo_namePic (make-object bitmap% "assets/logo_name.png"))
(define logoPic (make-object bitmap% "assets/logo.png"))
(define namePic (make-object bitmap% "assets/name.png"))
(define selectBackground (make-object bitmap% "assets/background_citySelect.png"))
(define startButtonIcon (make-object bitmap% "assets/start_button.png"))


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


;----VARIABLES FOR INITIALIZATION----;


; Main menu Screen
(define menuScreen (new frame% [label "Wazecheme"]
                   [width 1000]
                   [height 800]
                   [style '(no-resize-border)]))

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


;----METHODS----;

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

;----UI ELEMENTS----;

(new button% [parent menuPanel_bottom]
             [label (bitmap-scale startButtonIcon 0.6)]
             [callback (lambda (button event)
                         (toCitySelectScreen))])


;-------------------------City Select-------------------------;


;----VARIABLES FOR INITIALIZATION----;


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

; Draw elements in canvas
(define (drawSelect_left canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale selectBackground 0.25) 0 0))

(define (drawSelect_right canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale selectBackground 0.25) 0 -197))

; Main canvas
(define selectCanvas_left (new canvas% [parent selectPanel_left]
                                       [paint-callback drawSelect_left]))

(define selectCanvas_right (new canvas% [parent selectPanel_right]
                                        [paint-callback drawSelect_right]))


;----METHODS----;


; Changes screen to Arcadia Bay
(define (toArcadiaBay)
  (send citySelectScreen show #f)
  (send arcadiaBayScreen show #t))

; Changes screen to Custom City
(define (toCustomCity)
  (send citySelectScreen show #f)
  (send customCityScreen show #t))


;----UI ELEMENTS----;

(new button% [parent selectPanel_left]
             [label (bitmap-scale arcadiabayButtonIcon 0.54)]
             [callback (lambda (button event)
                         (toArcadiaBay))])

(new button% [parent selectPanel_right]
             [label (bitmap-scale customcityButtonIcon 0.6)]
             [callback (lambda (button event)
                         (toCustomCity)
                         (showCustomCityControls))])


;-------------------------Arcadia Bay-------------------------;


;----VARIABLES FOR INITIALIZATION----;


(define arcadiaBay_coordList '((97 80) (151 94) (167 58) (181 78)
                              (191 54) (310 30) (206 91) (190 115)
                              (183 125) (207 116) (244 116) (282 111) (181 171)
                              (218 158) (301 154) (329 157) (171 201) (237 189)
                              (302 184) (192 220) (233 213) (164 238) (232 241) (222 261)
                              (435 290)))

(define arcadiaBay_connectionList '(("the lighthouse" "beach/frank" "50" #t)
                                    ("beach/frank" "water tower" "15" #t)
                                    ("water tower" "the junkyard" "5" #t)
                                    ("the junkyard" "junkyard (parking)" "5" #t)
                                    ("junkyard (parking)" "wooded area" "35" #f)
                                    ("water tower" "jefferson" "10" #t)
                                    ("jefferson" "chuz" "5" #t)
                                    ("chuz" "church" "3" #t)
                                    ("church" "two whales diner" "2" #t)
                                    ("chuz" "price home" "8" #t)
                                    ("price home" "ale" "6" #t)
                                    ("ale" "parking lot" "10" #f)
                                    ("parking lot" "blackwell academy" "5" #t)
                                    ("parking lot" "ocram" "100" #t)
                                    ("chuz" "chloe" "15" #t)
                                    ("two whales diner" "butterfly cove" "10" #t)
                                    ("butterfly cove" "chloe" "6" #t)
                                    ("butterfly cove" "blackwell academy" "30" #t)
                                    ("butterfly cove" "harbor" "6" #t)
                                    ("harbor" "lumber mill" "8" #t)
                                    ("harbor" "gas station" "5" #t)
                                    ("gas station" "esteban" "7" #t)
                                    ("esteban" "prescott estate" "3" #t)
                                    ("chloe" "prescott estate" "6" #t)
                                    ("prescott estate" "ocram" "100" #t)
                                    ("esteban" "marmota" "1" #f)
                                    ("general store" "marmota" "5" #f)
                                    ("marmota" "the barn" "70" #f)
                                    ("lumber mill" "general store" "16" #t)
                                    ))

(define arcadiaBay_stringList '("the lighthouse" "beach/frank" "the junkyard" "water tower"
                              "junkyard (parking)" "wooded area" "jefferson" "church"
                              "two whales diner" "chuz" "price home" "ale" "butterfly cove"
                              "chloe" "parking lot" "blackwell academy" "harbor" "prescott estate"
                              "ocram" "gas station" "esteban" "lumber mill" "marmota" "general store"
                              "the barn"))

(define arcadiaBay_nodeList '())

; Arcadia Bay screen
(define arcadiaBayScreen (new frame% [label "Wazecheme"]
                                     [width 1000]
                                     [height 800]
                                     [style '(no-resize-border)]))

; Arcadia Bay panel
(define arcadiaBayPanel (new panel% [parent arcadiaBayScreen]
                                    [border 0]
                                    [spacing 0]
                                    [alignment '(right center)]))

; Draws Arcadia Bay elements in canvas
(define (drawArcadiaBay canvas dc)
  (send dc set-scale 2 2)
  (send dc draw-bitmap (bitmap-scale arcadiabay_map 0.25) 0 0)
  (initializeArcadiaBayNodeList arcadiaBay_stringList arcadiaBay_coordList)
  (drawArcadiaBayNodes arcadiaBay_nodeList)
  (drawArcadiaBayConnections arcadiaBay_connectionList))

; Arcadia Bay canvas and drawing context
(define arcadiaBayCanvas (new canvas% [parent arcadiaBayPanel]
                                      [paint-callback drawArcadiaBay]))

(define arcadiaBayDC (send arcadiaBayCanvas get-dc))


;----METHODS----;

; Draws all Arcadia Bay nodes in list
(define (drawArcadiaBayNodes list)
  (cond ((null? list) #f)
        (else (let* ([currentNode (car (car list))]
                     [currentPoint (getPoint_arcadiaBay currentNode)]
                     [currentX (send currentPoint get-x)]
                     [currentY (send currentPoint get-y)])
              (drawNodeInPos_arcadiaBay currentNode currentX currentY)
              (drawArcadiaBayNodes (cdr list))))))

; Draws all connections in list
(define (drawArcadiaBayConnections list)
  (cond ((null? list) #f)
        (else (let* ([currentConnection (car list)]
                     [currentN1 (car currentConnection)]
                     [currentN2 (car (cdr currentConnection))]
                     [currentWeight (car (cdr (cdr currentConnection)))]
                     [currentIsBidirectional (car (cdr (cdr (cdr currentConnection))))])
               (redrawArcadiaBayConnection currentN1 currentN2 currentWeight currentIsBidirectional)
               (drawArcadiaBayConnections (cdr list))))))

; Draws node in UI in specific position for Arcadia Bay
(define (drawNodeInPos_arcadiaBay n x y)
  (let* ([point (make-object point% x y)])
  (send arcadiaBayDC set-brush gold-brush)
  (send arcadiaBayDC set-pen gold-pen)
  (send arcadiaBayDC draw-ellipse x y 3 3)))

; Draws single route on Arcadia Bay
(define (drawArcadiaBayRoute route isShortest)
  (let* ([n1 (car route)]
         [nRoute (cdr route)])
  (cond ((null? nRoute) #f)
        (else (let* ([n2 (car nRoute)])
              (drawArcadiaBaySimpleConnection n1 n2 isShortest)
              (drawArcadiaBayRoute (cdr route) isShortest))))))

; Draws all found routes
(define (drawArcadiaBayRoutes routeList isShortest)
  (cond ((null? routeList) #f)
        (else (drawArcadiaBayRoute (car routeList) isShortest)
              (let* ([nextRoute (cdr routeList)])
              (cond ((equal? (length nextRoute) 1) (drawArcadiaBayRoutes nextRoute #t))
                    (else (drawArcadiaBayRoutes nextRoute #f)))))))

; Draws connection between two nodes without weight or bidirectionality
; Used to draw found routes
(define (drawArcadiaBaySimpleConnection n1 n2 isShortest)
  (let* ([p1 (getPoint_arcadiaBay n1)]
         [p2 (getPoint_arcadiaBay n2)]
         [p1x (send p1 get-x)]
         [p1y (send p1 get-y)]
         [p2x (send p2 get-x)]
         [p2y (send p2 get-y)]) 
  (send arcadiaBayDC set-pen red-pen)
  (send arcadiaBayDC set-brush red-brush)
  (cond ((equal? isShortest #t) (and (send arcadiaBayDC set-pen green-pen)
                                (send arcadiaBayDC set-brush green-brush))))
  (draw-arrow arcadiaBayDC (+ p1x 1.5) (+ p1y 1.5) (+ p2x 1.5) (+ p2y 1.5) 0 0)
  (send arcadiaBayDC set-pen black-pen)))

; Gets point from string in Arcadia Bay
(define (getPoint_arcadiaBay n)
  (getPoint_aux n arcadiaBay_nodeList))

; Initializes Arcadia Bay nodes
(define (initializeArcadiaBayNodeList stringList coordList)
  (cond ((null? stringList) #f)
        (else (let* ([currentString (car stringList)]
                     [currentPoint (car coordList)]
                     [currentX (car currentPoint)]
                     [currentY (cadr currentPoint)]
                     [point (make-object point% currentX currentY)])
              (set! arcadiaBay_nodeList (append arcadiaBay_nodeList (list (list currentString point))))
              (initializeArcadiaBayNodeList (cdr stringList) (cdr coordList))))))

; Organizes data for path drawing on ArcadiaBay
(define (pathHelperArcadiaBay)
  (let* ([origin (send routeOrigin_entry get-value)]
         [destination (send routeDestination_entry get-value)]
         [path (reverse (get-path origin destination arcadia-graph))])
  (drawArcadiaBayRoutes path #f)
  (redrawArcadiaBayWeights arcadiaBay_connectionList)))

; Redraws Arcadia Bay
(define (redrawArcadiaBay)
  (send arcadiaBayDC draw-bitmap (bitmap-scale arcadiabay_map 0.25) 0 0)
  (drawArcadiaBayNodes arcadiaBay_nodeList)
  (drawArcadiaBayConnections arcadiaBay_connectionList))

; Redraws connection between two graphical nodes in Arcadia Bay
(define (redrawArcadiaBayConnection n1 n2 weight isBidirectional)
  (let* ([p1 (getPoint_arcadiaBay n1)]
         [p2 (getPoint_arcadiaBay n2)]
         [p1x (send p1 get-x)]
         [p1y (send p1 get-y)]
         [p2x (send p2 get-x)]
         [p2y (send p2 get-y)]
         [middlePoint (getMiddlePoint p1x p1y p2x p2y)]
         [midX (send middlePoint get-x)]
         [midY (send middlePoint get-y)])
  (send arcadiaBayDC set-pen brushedBlue-pen_slim)
  (draw-arrow arcadiaBayDC (+ p1x 1.5) (+ p1y 1.5) (+ p2x 1.5) (+ p2y 1.5) 0 0)
  (cond ((equal? isBidirectional #t) (draw-arrow arcadiaBayDC (+ p2x 1.5) (+ p2y 1.5) (+ p1x 1.5) (+ p1y 1.5) 0 0)))
  (send arcadiaBayDC set-pen black-pen)
  (send arcadiaBayDC set-font (make-font #:size 6))
  (send arcadiaBayDC draw-text weight (- midX 5) (- midY 5))))

; Redraws all weights in graph
(define (redrawArcadiaBayWeights list)
  (cond ((not (null? list))
         (let* ([n1 (caar list)]
                [n2 (cadar list)]
                [weight (caddar list)]
                [p1 (getPoint_arcadiaBay n1)]
                [p2 (getPoint_arcadiaBay n2)]
                [p1x (send p1 get-x)]
                [p1y (send p1 get-y)]
                [p2x (send p2 get-x)]
                [p2y (send p2 get-y)]
                [middlePoint (getMiddlePoint p1x p1y p2x p2y)]
                [midX (send middlePoint get-x)]
                [midY (send middlePoint get-y)])
           (send arcadiaBayDC draw-text weight (- midX 5) (- midY 5))
           (redrawArcadiaBayWeights (cdr list))))))

;----UI ELEMENTS----;

(define routeOrigin_entry (new text-field%
                          (label "Origin")
                          (parent arcadiaBayScreen)
                          (init-value "")))

(define routeDestination_entry (new text-field%
                               (label "Destination")
                               (parent arcadiaBayScreen)
                               (init-value "")))

(new button% [parent arcadiaBayScreen]
             [label "Calculate"]
             [callback (lambda (button event)
                         (redrawArcadiaBay)
                         (pathHelperArcadiaBay)
                         (send routeOrigin_entry set-value "")
                         (send routeDestination_entry set-value ""))])

;-------------------------Custom City-------------------------;


;----VARIABLES FOR INITIALIZATION----;

(define nodeList '())
(define connectionList '())
(define customCityGraph '())

; Custom City screen and panel
(define customCityScreen (new frame% [label "Wazecheme"]
                                     [width 1000]
                                     [height 800]
                                     [style '(no-resize-border)]))

(define customCityScreenControls (new frame% [label "Control Panel"]
                                             [width 100]
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

; Check if connection is repeated
(define (checkRepeatedConnections n1 n2 list)
  (cond ((null? list) #f)
        (else (let* ([currentN1 (caar list)]
                     [currentN2  (cadar list)])
              (cond ((and (equal? n1 currentN1) (equal? n2 currentN2)) (updateStatus "Connection is already made") #t)
                    (else (checkRepeatedConnections n1 n2 (cdr list))))))))

; Check if node number is repeated
(define (checkRepeatedNodes n)
  (checkRepeatedNodes_aux n nodeList)
  (send addNode_entry set-value ""))

(define (checkRepeatedNodes_aux n list)
  (cond ((equal? n "") #f)
        ((null? list) (drawNode n))
        (else (cond ((equal? (car (car list)) n) #f)
                    (else (checkRepeatedNodes_aux n (cdr list)))))))

; Checks if weight is valid
(define (checkWeight weight)
  (cond ((number? (string->number weight)) #t)
        (else (updateStatus "Weight is not valid") #f)))

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
         [p2 (getPoint n2)])
  (cond ((equal? p1 #f) (updateStatus "Origin node doesn't exist"))
        ((equal? p2 #f) (updateStatus "Destination node doesn't exist"))
        (else (let* ([p1x (send p1 get-x)]
                       [p1y (send p1 get-y)]
                       [p2x (send p2 get-x)]
                       [p2y (send p2 get-y)]
                       [middlePoint (getMiddlePoint p1x p1y p2x p2y)]
                       [midX (send middlePoint get-x)]
                       [midY (send middlePoint get-y)])
                  ; Connect in graph
                  (connect n1 n2 (string->number weight) isBidirectional)
                  (set! connectionList (append connectionList (list (list n1 n2 weight isBidirectional))))
                  ; Draw arrow from p1 to p2
                  (send customCityDC set-pen brushedBlue-pen_slim)
                  (send customCityDC set-brush blue-brush)
                  (draw-arrow customCityDC (+ p1x 1.5) (+ p1y 1.5) (+ p2x 1.5) (+ p2y 1.5) 0 0)
                  (cond ((equal? isBidirectional #t) (draw-arrow customCityDC (+ p2x 1.5) (+ p2y 1.5) (+ p1x 1.5) (+ p1y 1.5) 0 0)))
                  (send customCityDC set-pen black-pen)
                  (send customCityDC set-text-foreground (make-object color% 205 60 236))
                  (send customCityDC draw-text weight (- midX 5) (- midY 5))
                  (send customCityDC set-text-foreground "black"))))))

; Draws node in UI (ADD NODE TO GRAPH)
(define (drawNode n)
  (let* ([x (random 15 480)]
         [y (random 15 380)]
         [point (make-object point% x y)])
  (send customCityDC set-brush gold-brush)
  (send customCityDC set-pen gold-pen)
  (send customCityDC draw-ellipse x y 3 3)
  (send customCityDC set-font (make-font #:size 8))
  (send customCityDC draw-text n x (- y 15))
  (graph-maker n)
  (set! nodeList (append nodeList (list (list n point))))))

; Draws node in UI in specific position
(define (drawNodeInPos n x y)
  (let* ([point (make-object point% x y)])
  (send customCityDC set-brush gold-brush)
  (send customCityDC set-pen gold-pen)
  (send customCityDC draw-ellipse x y 3 3)
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
              (let* ([nextRoute (cdr routeList)])
              (cond ((equal? (length nextRoute) 1) (drawRoutes nextRoute #t))
                    (else (drawRoutes nextRoute #f)))))))

; Draws connection between two nodes without weight or bidirectionality
; Used to draw found routes
(define (drawSimpleConnection n1 n2 isShortest)
  (let* ([p1 (getPoint n1)]
         [p2 (getPoint n2)]
         [p1x (send p1 get-x)]
         [p1y (send p1 get-y)]
         [p2x (send p2 get-x)]
         [p2y (send p2 get-y)]) 
  (send customCityDC set-pen red-pen)
  (cond ((equal? isShortest #t) (and (send customCityDC set-pen green-pen)
                                     (send customCityDC set-brush green-brush))))
  (draw-arrow customCityDC (+ p1x 1.5) (+ p1y 1.5) (+ p2x 1.5) (+ p2y 1.5) 0 0)
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

; Organizes data for path drawing
(define (pathHelper)
  (let* ([origin (send connectOrigin_entry get-value)]
         [destination (send connectDestination_entry get-value)]
         [path (get-path origin destination custom-graph)]
         [reversePath (reverse path)])
  (updateRoutes reversePath)
  (updateStatus (string-append "Shortest path weight: " (number->string (path-weight (car path) custom-graph))))
  (cond ((equal? (length reversePath) 1)
         (drawRoutes reversePath #t))
        (else (drawRoutes reversePath #f)))
  (redrawWeights connectionList)))

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
  (send customCityDC set-pen brushedBlue-pen_slim)
  (draw-arrow customCityDC (+ p1x 1.5) (+ p1y 1.5) (+ p2x 1.5) (+ p2y 1.5) 0 0)
  (cond ((equal? isBidirectional #t) (draw-arrow customCityDC (+ p2x 1.5) (+ p2y 1.5) (+ p1x 1.5) (+ p1y 1.5) 0 0)))
  (send customCityDC set-pen black-pen)
  (send customCityDC set-text-foreground (make-object color% 205 60 236))
  (send customCityDC draw-text weight (- midX 5) (- midY 5))
  (send customCityDC set-text-foreground "black")))

; Redraws custom graph
(define (redrawGraph)
  (send customCityDC draw-bitmap (bitmap-scale customcity_map 0.25) 0 0)
  (drawAllNodes nodeList)
  (drawAllConnections connectionList))

; Redraws all node names in custom graph
(define (redrawNodeNames list)
  (cond ((not (null? list))
         (let* ([name (caar list)]
                [currentPoint (cadar nodeList)]
                [currentX (send currentPoint get-x)]
                [currentY (send currentPoint get-y)])
           (send customCityDC draw-text name currentX (- currentY 15))
           (redrawNodeNames (cdr list))))))


; Redraws all weights in custom graph
(define (redrawWeights list)
  (cond ((not (null? list))
         (let* ([n1 (caar list)]
                [n2 (cadar list)]
                [weight (caddar list)]
                [p1 (getPoint n1)]
                [p2 (getPoint n2)]
                [p1x (send p1 get-x)]
                [p1y (send p1 get-y)]
                [p2x (send p2 get-x)]
                [p2y (send p2 get-y)]
                [middlePoint (getMiddlePoint p1x p1y p2x p2y)]
                [midX (send middlePoint get-x)]
                [midY (send middlePoint get-y)])
           (send customCityDC set-text-foreground (make-object color% 205 60 236))
           (send customCityDC draw-text weight (- midX 5) (- midY 5))
           (redrawWeights (cdr list))))))

(define (showCustomCityControls)
  (send customCityScreenControls show #t))

(define (updateRoutes path)
  (send routeBox clear)
  (updateRoutes_aux path))

(define (updateRoutes_aux path)
  (cond ((null? path) #f)
        (else (send routeBox append (~a (car path)))
              (updateRoutes_aux (cdr path)))))
  
(define (updateStatus nStatus)
  (send statusLabel set-label nStatus))


;----UI ELEMENTS----;


(define addNode_entry (new text-field%
                      (label "Node name")
                      (parent customCityScreenControls)
                      (init-value "")))

(new button% [parent customCityScreenControls]
             [label addButtonIcon]
             [callback (lambda (button event)
                        (let* ([status (checkRepeatedNodes (send addNode_entry get-value))])
                          (cond ((equal? status #f) (updateStatus "Node name is already taken")))))])

(define connectOrigin_entry (new text-field%
                            (label "Origin")
                            (parent customCityScreenControls)
                            (init-value "")))

(define connectDestination_entry (new text-field%
                                 (label "Destination")
                                 (parent customCityScreenControls)
                                 (init-value "")))

(define connectWeight_entry (new text-field%
                            (label "Weight/Distance")
                            (parent customCityScreenControls)
                            (init-value "")))

(define isBidirectional (new check-box%
                        (parent customCityScreenControls)
                        (label "Bidirectional?")
                        (value #f)))

(define statusLabel (new message%
                         [label "You've met a terrible fate, haven't you?"]
                         [parent customCityScreenControls]))

(new button% [parent customCityScreenControls]
             [label connectButtonIcon]
             [callback (lambda (button event)
                       (let* ([origin (send connectOrigin_entry get-value)]
                              [destination (send connectDestination_entry get-value)]
                              [weight (send connectWeight_entry get-value)])
                       (cond ((and (equal? (checkRepeatedConnections origin destination connectionList) #f)
                                   (equal? (checkWeight weight) #t))
                              (drawConnection origin
                                              destination
                                              weight
                                              (send isBidirectional get-value))))
                       (send connectOrigin_entry set-value "")
                       (send connectDestination_entry set-value "")
                       (send connectWeight_entry set-value "")
                       (send isBidirectional set-value #f)))])

(new button% [parent customCityScreenControls]
             [label findRouteButtonIcon]
             [callback (lambda (button event)
                         (let* ([origin (send connectOrigin_entry get-value)]
                              [destination (send connectDestination_entry get-value)])
                           (cond ((or (equal? (checkRepeatedNodes origin) #f)
                                      (equal? (checkRepeatedNodes destination) #f))
                                  (updateStatus "Error. A node doesn't exist"))
                           (else (redrawGraph)
                                 (pathHelper)
                                 (send connectOrigin_entry set-value "")
                                 (send connectDestination_entry set-value "")))))])

(define routeBox (new list-box%
                         [parent customCityScreenControls]
                         [choices '()]
                         [label "Routes"]))


;-------------------------General-------------------------;


;----VARIABLES----;


(define black-pen
  (new pen%
       [color "black"]
       [width 2]))

(define blue-brush
  (new brush%
       [color "blue"]))

(define blue-pen
  (new pen%
       [color "blue"]
       [width 2]))

(define blue-pen_slim
  (new pen%
       [color "blue"]
       [width 1]))

(define brushedBlue-pen
  (new pen%
       [color (make-object color% 48 91 149)]
       [width 3]))

(define brushedBlue-pen_slim
  (new pen%
       [color (make-object color% 48 91 149)]
       [width 1]))

(define gold-brush
  (new brush%
       [color "gold"]))

(define gold-pen
  (new pen%
       [color "gold"]
       [width 3]))

(define green-brush
  (new brush%
       [color (make-object color% 40 70 50)]))

(define green-pen
  (new pen%
       [color (make-object color% 40 70 50)]
       [width 3]))

(define red-pen
  (new pen%
       [color (make-object color% 200 64 52)]
       [width 2]))

(define red-brush
  (new brush%
       [color (make-object color% 200 64 52)]))

(define transparent-brush
  (new brush%
       [style 'transparent]))


;----ACTIONS----;


(showMenu #t)