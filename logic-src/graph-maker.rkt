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

(provide custom-graph
         graph-maker
         connect
         remove-node
)

(require "../util-src/util.rkt"
         "graph-lib.rkt")

;; Graph customized by the user
> (define custom-graph '())

;; (graph-maker node)
;; Function that creates a graph from scratch dynamically, is taken as a list
;;of points without any association.
;; This function makes use of the primitive set!

;; -------------------------------------------------------------------------
;; If is a null graph produces: '( (node () ) )
;; else: '( (newNode () ) ... (node2 (ng1...ngN)) (node1 (ng1...ngN)) (node0
;;        (ng1...ngN)))
;; -------------------------------------------------------------------------

> (define(graph-maker node)
     (cond ((null? custom-graph)
              (set! custom-graph (list (list node '()))))
           (else
             (set! custom-graph (list* (list node '()) custom-graph)))))


;; (remove-node node graph)
;; Remove a node from the graph.
> (define (remove-node node graph)
      (cond ((null? graph)
              '())
            ((equal? node (caar graph))
              (cdr graph))
            (else (cons (car graph)
                  (remove-node node (cdr graph))))))


;; (connect node1 node2 weight bidirectional)
;; This function is used to connect two points in the graph, it is also defined
;; if your connection is going to be bidirectional or not.
;; In addition to this the weight (distance) between the connected nodes will
;; be defined.
> (define (connect node1 node2 weight bidirectional)
     (cond ( (and (equal? (member-node? node1 custom-graph) #t)
                  (equal? (member-node? node2 custom-graph) #t))
                      (cond ((equal? bidirectional #t)

                              #|----------------------------------------------
                                            CONNECTION ALGORITHM
                                1. Add the new neighbor to the node's current
                                    neighbor list.
                                2. Build a new "node" with the new list of
                                   neighbors.
                                3. The old node of the graph is removed.
                                4. The graph is joined with the new node.

                              Developed by @shuzz22260427 and @estalvgs1999
                              ---------------------------------------------- |#
                              (set! custom-graph
                                    (cons (list node1
                                          (add-neighbor node1 node2 weight
                                                        custom-graph))
                                          (remove-node node1 custom-graph)))
                              (set! custom-graph
                                    (cons (list node2
                                          (add-neighbor node2 node1 weight
                                                        custom-graph))
                                          (remove-node node2 custom-graph))))
                      (else
                        (set! custom-graph
                              (cons (list node1
                                    (add-neighbor node1 node2 weight
                                                  custom-graph))
                                    (remove-node node1 custom-graph))))))
      (else
        (print "Error 404 - The nodes you are looking for do not belong to this
        graph or you have entered a null graph"))))

;; (add-neighbor node1 node2 weight graph)
;; Add a neighbor to a node's list of neighbors
> (define (add-neighbor node1 node2 weight graph)
      (cond ((member-node? node2 (node? node1 graph))
              (print "Error 101 - A connection between these nodes already exists")
              (+ 100 1)
              (node? node1 graph))
      (else
        (cons (list node2 weight) (node? node1 graph)))))
