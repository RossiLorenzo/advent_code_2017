input = readLines("Day2/Input2_1.txt")
input_by_row = strsplit(input, "\t")

###################### PART 1 ######################
sum(sapply(input_by_row, function(x) {
  max(as.numeric(x)) - min(as.numeric(x))
}))

###################### PART 2 ######################
sum(sapply(input_by_row, function(x) {
  tmp = expand.grid(as.numeric(x), as.numeric(x))
  tmp = tmp[tmp[,1] %% tmp[,2] == 0 & tmp[,1] != tmp[,2], ]
  tmp[1,1] / tmp[1,2] 
}))