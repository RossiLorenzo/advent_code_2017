input = readLines("Day6/Input6_1.txt")
input = as.numeric(strsplit(input, "\t")[[1]])
length_input = length(input)

###################### PART 1 ######################
# Function to run one interaction
one_interaction = function(config){
  max_pos = min(which(config == max(config)))
  max_val = config[max_pos]
  config[max_pos] = 0
  affected_pos = (1:max_val + max_pos - 1) %% length_input + 1
  if(max_val <= length_input){
    config[affected_pos] = config[affected_pos] + 1
    return(config)
  }
  return(as.numeric(config + table(affected_pos)))
}
# Loop 
counter = 0
config = input
seen_configs = NULL
while(!paste0(config, collapse = "-") %in% seen_configs){
  seen_configs = c(seen_configs, paste0(config, collapse = "-"))
  config = one_interaction(config)
  counter = counter + 1
}
counter

###################### PART 2 ######################
counter - which(seen_configs %in% paste0(config, collapse = "-")) + 1
