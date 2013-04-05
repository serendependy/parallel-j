p_rank =: "

NB. If the two items are in order, return them
NB. otherwise, reverse their order
inOrder_2 =: <:/
reverse =: |.
identity =: ]
sort2 =: reverse`identity@.inOrder_2

NB. Turn rank1 array with 2^n elements
NB. Into rank n array, shape n copies of 2
NB. e.g., array of 8 => array of 2 2 2
divide =: $~ 2 #~ 2 ^. #

NB. Divide into different rank 1 cells (rows of 2 each)
NB. Sort each of these rank 1 cells
NB. Then, merge the rank 1 cells pairwise
NB. as many times as 1 less than the dimension of the rank
dim    =: #@$
repeated =: ^:
repeatedMerge =: merge/(p_rank 2) repeated (dim - 1:)
mergeSort =: repeatedMerge@:(sort2 p_rank 1)@:divide
