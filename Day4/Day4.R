library(dplyr)
input = readLines("Day4/Input4_1.txt")
input_split = strsplit(input, " ", fixed = T)

###################### PART 1 ######################
length(Filter(function(x){ length(x) == length(unique(x))}, input_split))

###################### PART 2 ######################
# First remove the words that are certainly not anagrams because of the length
length(Filter(function(x){
  tmp = Map(function(y){ 
    tmp = table(y)
    ret = as.data.frame.matrix(matrix(tmp, nrow = 1))
    names(ret) = names(tmp)
    return(ret)
  }, strsplit(x, "", fixed = T)) %>% 
    bind_rows()
  nrow(tmp) == nrow(distinct(tmp))
}, input_split))
