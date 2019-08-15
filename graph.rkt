#lang racket
#|------------------------------------------------------------------------------
                    WAZESCHEME MAP IMPLEMENTED AS A GRAPH

  The graph structure is defined as:
  ( (node1 (neighbour1 neighbour2 ... neighbourN )) ... (nodeN(ng1 ...ngN))))

  A graph has elementary operations:

    * solution? - Indicates if a given path is a solution for the graph
    * neighbors? - Indicate if two nodes are neighbors
    * get-neighbors - Returns all neighbors of a given node
    * extend - Find new paths from a certain path
    * weight-btw-nodes - Returns the weight between two nodes
    * path-weight - Returns the weight of a given path
    * node? - Look for a node in a graph and if it finds it returns its neighbors
    * DFS - Search for a route by searching in depth
    * BFS - Search for a route by depth search by width
    * DFS-ALL - Search all paths by DFS
    * BFS-ALL - Search all paths by BFS

  The functions implemented were inspired by the book: "Introducción a la programación en Scheme"(José E. Helio Guzmán)

  Developed by @estalvgs1999
------------------------------------------------------------------------------|#

(define graph
    '(
     (a ((b 5) (c 8)))
     (b ((c 6) (d 7)))
     (c ((a 9) (d 2)))
     (d ((b 8) (a 1)))
     )
)

;; (solution? end path)
;; Indicates if a given path is a solution for the graph
> (define (solution? end path)
    (equal? end (car path)))

;; (neighbors? node1 node2 graph)
;; Indicate if two nodes are neighbors
> (define (neighbors? node1 node2 graph)
    (cond ((null? graph)
            #f)
          ((equal? node1 (caar graph))
            (neighbors-aux node2 (cadar graph)))
          (else
            (neighbors? node1 node2 (cdr graph)))))

> (define (neighbors-aux node direct-ngbs)
    (cond ((null? direct-ngbs)
            #f )
          ((equal? node (caar direct-ngbs))
            #t)
          (else
            (neighbors-aux (cdr direct-ngbs)))))

;; (get-neighbours node graph)
;; Returns all neighbors of a given node

> (define (get-neighbors node graph)
    (cond ((null? graph)
            '())
          ((equal? node (caar graph))
            (cadar graph))
          (else
            (get-neighbors node (cdr graph)))))

;; (extend path graph)
;; Find new paths from a certain path
;; Check if the element exists in the partial route, to avoid cyclical evaluations

> (define (extend path graph)
    (extend-aux (get-neighbors (car path) graph) '() path))

> (define (extend-aux neighbors result path)
    (cond ((null? neighbors)
            result)
          ;; Check if the element exists in the partial route, to avoid
          ;; cyclical evaluations
          ((member (car neighbors) path)
            (extend-aux (cdr neighbors) result path))
          (else
            (extend-aux (cdr neighbors)
            (append result (list(list* (caar neighbors) path)))
            path ))))
