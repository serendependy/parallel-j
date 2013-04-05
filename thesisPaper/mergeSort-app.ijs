sort2 =: |.`]@.(<:/)
divide =: $~ 2 #~ 2 ^. #
dim    =: #@$
mergeSort =: merge/("2) ^: (dim - 1:))@:(sort2"1)@:divide
