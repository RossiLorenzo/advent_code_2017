input = as.numeric(readLines("Day17/Input17_1.txt"))

###################### PART 1 ######################
pos_array = c(0, 1, rep(NA, 2015))
mypos = 2
myvalue = 2
while(myvalue <= 2017){
  mypos = (mypos + input - 1) %% myvalue + 1
  if(mypos != myvalue)
    pos_array[(mypos + 2) : (myvalue + 1)] = pos_array[(mypos + 1) : myvalue]
  pos_array[mypos + 1] = myvalue
  myvalue = myvalue + 1
  mypos = mypos + 1
  values_in_pos_2 = c(values_in_pos_2, pos_array[2])
}
pos_array[which(pos_array == 2017) + 1]


###################### PART 2 ######################
# It is obvious that 0 will always stay in position 1 since there is no way to move it.
# Therefore looking for the number after the 0 is simply looking at what is in position 2
# At this point I do not need to remeber the position of all the elements but just:
# what is in position 2 and the active position
mypos = 2
myvalue = 2
value_in_pos_2 = 1
while(myvalue <= 50000000){
  mypos = (mypos + input - 1) %% myvalue + 1
  if(mypos == 1)
    value_in_pos_2 = myvalue
  myvalue = myvalue + 1
  mypos = mypos + 1
}
value_in_pos_2
