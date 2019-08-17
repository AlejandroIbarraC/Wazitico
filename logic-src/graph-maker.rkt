#|------------------------------------------------------------------------------
                          WAZESCHEME GRAPH MAKER

  This module provides the logical functions necessary to interact with the
  interface in a dynamic and static way.
  Functions are provided to create a graph from scratch and connect its points.

  Methods implemented:
      * graph-maker - Function that is responsible for building graphs by
                      adding nodes.
      * connect - Function that makes the connection enters two nodes.
      * remove-node - Remove a node from the graph.

  [!] IMPORTANT ***************************************************************
  THIS MODULE HANDLES A 𝗚𝗥𝗔𝗣𝗛 𝗔𝗦 𝗚𝗟𝗢𝗕𝗔𝗟 𝗩𝗔𝗥𝗜𝗔𝗕𝗟𝗘, ALL THE
  ACTIONS PROVIDED ARE PERFORMED ON IT.
  THE MODULE PROVIDES THE VARIABLE GRAPH TO BE USED IN OTHER FILES OF THE
  PROJECT.
  *****************************************************************************

  The functions implemented were inspired by the book: "Introducción a la
  programación en Scheme"(José E. Helio Guzmán)

  Developed by @shuzz22260427
------------------------------------------------------------------------------|#
#lang racket

(provide ;;custom-graph
         grafo2
         graph-maker
         connect
         remove-node)

;; Graph customized by the user
;; JESUS! PONGALE A ESTE MAN "custom-graph" y llamelo así en las demás funciones
>(define grafo2 '())

;; (graph-maker node)
;; Function that creates a graph from scratch dynamically, is taken as a list
;;of points without any association.
;; This function makes use of the primitive set!
>(define(crearGrafo nodo)
   (cond ( (null? grafo2)
           (set! grafo2 (cons nodo grafo2)))
         (else
          (set! grafo2 (cons nodo grafo2)))))


;; (remove-node node graph pivot)
;; Remove a node from the graph. [ESTA DE VERDAD SE USA?]
> (define (eliminar num lista pivote)
    (cond((null? lista)
          lista)
        ((and (equal? num (car lista)) (= pivote 0))
          (cdr lista)
          (eliminar num lista (+ pivote 1)))
        (else
          (cons(car lista)
          (eliminar num (cdr lista) pivote)))))


;; (connect node1 node2 weight bidirectional)
;; This function is used to connect two points in the graph, it is also defined
;; if your connection is going to be bidirectional or not.
;; In addition to this the weight (distance) between the connected nodes will
;; be defined.
> (define (conexiones nodo1 nodo2 pesoNodos bidireccional)
     (cond ( (and (equal? (miembro nodo1 grafo2) #t)
                  (equal? (miembro nodo2 grafo2) #t))
                      (cond ((equal? bidireccional #t)
                              (set! grafo2
                                    (cons (list nodo1 (list nodo2 pesoNodos))
                                          grafo2))
                              (set! grafo2
                                    (cons (list nodo2 (list nodo1 pesoNodos))
                                          grafo2)))
                      (else
                        (set! grafo2
                              (cons (list nodo1 (list nodo2 pesoNodos))
                                    grafo2)))))
      (else
        "Los elementos no pertenecen al grafo o grafo nulo")))
