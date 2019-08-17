#| ----------------------------------------------------------------------------
                        ARCADIA BAY MAP/GRAPH
  Designed by @AlejandroIbarraC and implemented by @estalvgs1999
 -----------------------------------------------------------------------------|#
#lang racket

(provide arcadia-graph)

;; ArcadiaBay Graph
> (define arcadia-graph
    '(
      ("the lighthouse"
        (
          ("beach/frank" 50)
        )
      )

      ("beach/frank"
        (
          ("the lighthouse" 50)
          ("water tower" 15)
        )
      )

      ("water tower"
        (
          ("beach/frank" 15)
          ("the junkyard" 5)
          ("jefferson" 10)
        )
      )

      ("the junkyard"
        (
          ("water tower" 5)
          ("junkyard (parking)" 5)
        )
      )

      ("junkyard (parking)"
        (
          ("the junkyard" 5)
        )
      )

      ("jefferson"
        (
          ("water tower" 10)
          ("chuz" 5)
        )
      )

      ("chuz"
        (
          ("jefferson" 5)
          ("church" 3)
          ("price home" 8)
          ("chloe" 15)
        )
      )

      ("church"
        (
          ("chuz" 3)
          ("two whales diner" 2)
        )
      )

      ("two whales diner"
        (
          ("church" 2)
          ("butterfly cove" 10)
        )
      )

      ("price home"
        (
          ("chuz" 8)
          ("ale" 6)
        )
      )

      ("ale"
        (
          ("parking lot" 10)
        )
      )

      ("parking lot"
        (
          ("ale" 10)
          ("blackwell academy" 5)
          ("ocram" 100)
        )
      )

      ("blackwell academy"
        (
          ("parking lot" 5)
          ("butterfly cove" 30)
        )
      )

      ("ocram"
        (
          ("parkin lot" 100)
          ("prescott estate" 100)
        )
      )

      ("chloe"
        (
          ("chuz" 15)
          ("butterfly cove" 6)
          ("prescott estate" 6)
        )
      )

      ("butterfly cove"
        (
          ("two whales diner" 10)
          ("chloe" 6)
          ("blackwell academy" 30)
          ("harbor" 6)
        )
      )

      ("harbor"
        (
          ("butterfly cove" 6)
          ("lumber mill" 8)"prescott estate" "6"
          ("gas station" 5)
        )
      )

      ("lumber mill"
        (
          ("harbor" 8)
          ("general store" 16)
        )
      )

      ("gas station"
        (
          ("harbor" 5)
          ("esteban" 7)
        )
      )

      ("esteban"
        (
          ("gas station" 7)
          ("prescott estate" 3)
          ("marmota" 1)
        )
      )

      ("prescott estate"
        (
          ("esteban" 3)
          ("chloe" 6)
          ("ocram" 100)
        )
      )

      ("marmota"
        (
          ("the barn" 70)
        )
      )

      ("general store"
        (
          ("marmota" 5)
          ("lumber mill" 16)
        )
      )

      ("the barn"
        (

        )
      )

     )
  )
