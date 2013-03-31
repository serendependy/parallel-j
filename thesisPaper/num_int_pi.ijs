func =: 1 (div =: %) (1 + (square =: *:))
xs =: integers div (n =: ])
num_int_pi =: (4 * (sum =: +/) @ func @ xs) div n
