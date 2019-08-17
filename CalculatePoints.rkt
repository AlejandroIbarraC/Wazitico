#lang racket
"Funcion para diagramar 'flechas' en el grafo
recibe un punto x,y el cual se representa en el
grafo como el punto final. Con este punto se consiguen
otros dos pares de puntos para formar la cabeza de una flecha"
>(define(calculatePoints x y)
   (cons (list (+ x 1) y) (list x (- y 1))))
>(calculatePoints 2 5)
