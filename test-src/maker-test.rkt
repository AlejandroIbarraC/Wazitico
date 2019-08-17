#lang racket


(require "../logic-src/graph-lib.rkt"
         "../logic-src/graph-maker.rkt"
         "../logic-src/path-engine.rkt")

> (graph-maker "Alajuela")
> custom-graph
> (graph-maker "San Jose")
> (graph-maker "Cartago")
> (graph-maker "Heredia")
> (graph-maker "Guanacaste")
> (graph-maker "Puntarenas")
> (graph-maker "Limon")
> custom-graph
> (connect "Alajuela" "Heredia" 2 #t)
> (connect "Alajuela" "San Jose" 11 #t)
> (connect "Alajuela" "Guanacaste" 20 #t)
> (connect "San Jose" "Cartago" 11 #t)
> (connect "San Jose" "Heredia" 7 #t)
> (connect "San Jose" "Limon" 50 #t)
> (connect "San Jose" "Cartago" 11 #t)
> custom-graph
> (get-path "Alajuela" "Limon" custom-graph)