NB. General algorithm for finding
NB. shifts of board of arbitrary rank
decr =: <: NB. decrement
integers =: i.
dim =: #@$
base =: #:
n =: ]
copies =: #
memoized =: M.

shiftVals =: decr@(n base integers@(* insert)) memoized
shiftBy =: shiftVals@:(dim copies 3:)

NB. Updated to use shiftBy as function, not data
neighborArray =: shiftBy (shift"1 _) n
listNeighbors =: (+ p_insert)@:neighborArray - n

NB. Rules of the game, as an array
NB.      0 (cell dead)   1 (cell alive)
rules =: (3 = ])       ` ([: +./ 2 3 = ])
                  
NB. Uses cell state to index into rule to apply
NB. appliedBy =: @.
nextState =: rules @. (cell =: [)("0)(p_rank 1) listNeighbors
