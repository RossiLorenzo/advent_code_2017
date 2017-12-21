input = as.numeric(readLines("Day17/Input17_1.txt"))

###################### PART 1 ######################
pos_array = c(0, 1, rep(NA, 2015))
mypos = 2
myvalue = 2
while(myvalue <= 2017){
  mypos = (mypos + input - 1) %% myvalue + 1
  if(mypos == myvalue){
    pos_array = c(pos_array, myvalue)
  }else{
    pos_array[(mypos + 2) : (myvalue + 1)] = pos_array[(mypos + 1) : myvalue]
    pos_array[mypos + 1] = myvalue
  }
  myvalue = myvalue + 1
  mypos = mypos + 1
}
pos_array[which(pos_array == 2017) + 1]
# 1547
###################### PART 2 ######################
pos_array = c(0, 1)
final_array = rep(NA, 2017)
mypos = 2
myvalue = 2
F3D.Utilities::profiling({
while(myvalue <= 100000){
  mypos = (mypos + input - 1) %% length(pos_array) + 1
  if(mypos == length(pos_array)){
    pos_array = c(pos_array, myvalue)
  }else{
    pos_array = c(pos_array[1:mypos], myvalue, pos_array[(mypos + 1):length(pos_array)])  
  }
  myvalue = myvalue + 1
  mypos = mypos + 1
}
})
pos_array[which(pos_array == 2017) + 1]
