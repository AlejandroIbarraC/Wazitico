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
# lang racket
