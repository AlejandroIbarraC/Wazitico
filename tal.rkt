#lang racket
"Principios basicos de los grafos, en este documento se crearan las funciones
logicas necesarias para interactuar con la interfaz de forma dinamica y estatica.
Se haran funciones para crear un grafo desde cero, conectar sus puntos y conseguir
la ruta mas corta por medio de pesos"

"Definir el grafo incial con el cual se trabajara
como una variable de tipo lista"
>(define grafo '( (i (a b))
                   (a (i c d))
                   (b (i c d))
                   (c (a b x))
                   (d (a b f))
                   (x (c))
                   (f (d))
                ))
"Crear una funcion que verifica que las rutas existan, mas no veririca eficiencia,
da #t en caso de que la ruta exista"
>(define(solucion fin ruta)
   (equal? fin (car ruta)))

;Prueba de dicha funcion
>(solucion 'f '(f a r d))

"Funcion vecinos de un grafo, se usa para obtener la adyacencia
o nodos vecinos de un grafo"
>(define(vecinos ele grafo)
   (cond ( (null? grafo)
           grafo)
         ( (equal? ele (caar grafo))
           (cdr grafo)
           (cdar grafo))
         (else
          (vecinos ele (cdr grafo)))))
>(vecinos 'd grafo)
"Funcion que crea un grafo desde cero de forma dinamica, se toma
como una lista de punto sin ningun tipo de asociacion.
Esta funcion hace uso de la primitiva set! debido a que
nuestro grafo es una variable a la cual se le asigna el tipo
lista"

>(define grafo2 '2)

>(define(crearGrafo nodo grafo2)
   (cond ( (null? grafo2)
           (set! grafo2 (list nodo grafo2)))
         (else
          (set! grafo2 (cons nodo grafo2)))))

"Pruebas de la funcion crear grafos por medio
del uso de la primitiva ;cons; "
>(set! grafo2 (list 3 grafo2))
grafo2
>(set! grafo2 (cons 4 grafo2))
grafo2
(set! grafo2 (cons 5 grafo2))
grafo2

"Funcion que verifica si un elemento pertenece a una lista
en caso de pertenecer devuelve un booleano #t"
>(define(miembro num lista)
   (cond( (null? lista)
          #f)
        ( (equal? num (car lista))
          #t)
        (else
         (miembro num (cdr lista)))))

"Funcion que elimina el mimebro de una lista"
>(define(eliminar num lista pivote)
   (cond( (null? lista)
         lista)
        ( (and (equal? num (car lista)) (= pivote 0))
          (cdr lista)
          (eliminar num lista (+ pivote 1)))
        (else
         (cons(car lista)
         (eliminar num (cdr lista) pivote)))))

   
"Esta funcion se usa para conectar dos puntos del grafo creado
anteriormente, ademas se define si su conexion va a ser bidireccional
o solo en una direccion.
Ademas de esto se va a definir el peso(distancia) entre los nodos conectados.
El resultado de esta funcion es una lista con sublistas que son las conexiones
o rutas y los elementos que conforman el grafo original"
>(define(conexiones nodo1 nodo2 pesoNodos bidireccional grafo2)
   (cond ( (and (equal? (miembro nodo1 grafo2) #t) (equal? (miembro nodo2 grafo2) #t))
           (cond( (equal? bidireccional #t)
                  (set! grafo2 (cons (list nodo1 (list nodo2 pesoNodos)) grafo2))
                  (set! grafo2 (cons (list nodo2 (list nodo1 pesoNodos)) grafo2)))
                (else
                 (set! grafo2 (cons (list nodo1 (list nodo2 pesoNodos)) grafo2)))))
         (else
          "Los elementos no pertenecen al grafo o grafo nulo")))

>(conexiones 1 2 5 #t '(1 2 3 4))
>(define nodox 3)
>(define nodoy 4)
(set! grafo2 (cons (list nodox (list nodoy)) grafo2))
(set! grafo2 (cons (list nodoy (list nodox)) grafo2))
grafo2

"Distancia mas corta entre dos puntos. Esta funcion se usa
para, con base en los pesos, obtener la distancia mas corta
entre dos puntos del grafo, ademas de esto se obtienen las rutas
extra para que el usuario pueda visualizar la diferencia y eficiencia entre rutas"
> (define (DFS start end graph)
    (cond ((equal? start end)
            (list start))
          (else
            (DFS-aux (list (list start)) end graph))))

> (define (DFS-aux paths end graph)
    (cond ((null? paths)
            '())
          ((equal? end (caar paths))
            (reverse (car paths)))
          (else
            (DFS-aux (append (extend (car paths) graph)
                             (cdr paths))
                     end
                     graph))))
   