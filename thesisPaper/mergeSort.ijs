NB. Taken from the J Phrase Book
mrg =: 1 : '/:@/:@(m " _) { ,'

merge =: 4 : 0
b =. x interleaveOrdered y
y (b mrg) x
)
interleaveOrdered =: i.@:+&# e. (+ i.@:#)@:(+/"1@:>("0 1))

sort2 =: |.`]@.(<:/)

divide =: $~ 2 #~ 2 ^. #
dim    =: #@$
mergeSort =: merge/("2) ^: (dim - 1:))@:(sort2"1)@:divide
