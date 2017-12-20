Rcpp::sourceCpp("Day15/to_bit.cpp")
input = as.numeric(gsub(".* ", "", readLines("Day15/Input15_1.txt")))

###################### PART 1 ######################
day15_part1(input[1], input[2])

###################### PART 2 ######################
day15_part2(input[1], input[2])
