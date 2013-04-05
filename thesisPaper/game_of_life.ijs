p_rank =: "
p_insert =: /

NB. No neighbors at edge
shift =: |. !. 0 

NB. Specifies neighbors
NB. D=Down, U=Up, R=Right, L=Left
NB.              UL       U      UR    L      R     DL     D     DR
shiftBy =: 8 2 $ _1 _1   _1 0   _1 1   0 _1   0 1   1 _1   1 0   1 1 

NB. Generates higher-rank array for counting neighbors
neighborArray =: shiftBy&(shift " 1 _)

NB. Sums the neighbor values, finding how many are alive
listNeighbors =: (+ p_insert)@: neighborArray

NB. Rules of the game, as an array
NB.      0 (cell dead)   1 (cell alive)
rules =: (3 = ])       ` ([: +./ 2 3 = ])

NB. Uses cell state to index into rule to apply
NB. appliedBy =: @.
nextState =: rules @. (cell =: [)("0)(p_rank 1) listNeighbors
