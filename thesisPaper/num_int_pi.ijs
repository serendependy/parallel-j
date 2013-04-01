p_rank   =: " NB. Would be "::
p_insert =: / NB. Would be /::
integers =: i.

func =: (4 (div =: %) (1 + (square =: *:))) p_rank 0
xs =: integers div (n =: ])
num_int_pi =: (sum =: + p_insert)@:func@xs div n
