input = as.numeric(readLines("Day5/Input5_1.txt"))

###################### PART 1 ######################
position = 1
input_modify = input
steps = 1
while(TRUE){
  new_position = position + input_modify[position]
  if(new_position > length(input_modify))
    break()
  input_modify[position] = input_modify[position] + 1
  position = new_position
  steps = steps + 1
}
steps

###################### PART 2 ######################
position = 1
input_modify = input
steps = 1
while(TRUE){
  new_position = position + input_modify[position]
  if(new_position > length(input_modify))
    break()
  if(input_modify[position] >= 3){
    input_modify[position] = input_modify[position] - 1 
  }else{
    input_modify[position] = input_modify[position] + 1
  }
  position = new_position
  steps = steps + 1
}
steps