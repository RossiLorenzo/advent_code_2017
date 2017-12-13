library(tidyverse)
input = readLines("Day8/Input8_1.txt")
input_df = strsplit(input, " ", fixed = T) %>% 
  map_df(function(i){
    data.frame(
      Node = paste0("day8_", i[1]),
      Operation = paste0(paste0("day8_", i[1]), " = ", paste0("day8_", i[1]), ifelse(i[2] == "inc", "+", "-"), i[3]),
      Condition = paste0("day8_", i[5], " ", i[6], " ", i[7]),
      stringsAsFactors = F
    )
  })

###################### PART 1 & 2 ######################
my_env = new.env()
for(i in unique(input_df$Node))
  assign(i, 0, envir = my_env)
m = 0
for(i in 1:nrow(input_df)){
  if(eval(parse(text = input_df$Condition[i]), envir = my_env)){
    eval(parse(text = input_df$Operation[i]), envir = my_env)
  }
  all_values = eval(parse(text = "sapply(ls(), get, envir = my_env)"), envir = my_env)
  if(max(all_values) >= m)
    m = max(all_values)
}
print(max(all_values))
print(m)
