input = strsplit(readLines("Day11/Input11_1.txt"), ",")[[1]]

###################### PART 1 ######################
coord = c(0, 0) # row and column number 
pos = "upper"
update_pos = function(L){
  coord = L[[1]]
  pos = L[[2]]
  dir = L[[3]]
  new_coord = coord
  # North and South are easy
  if(dir == "n")
    new_coord[1] = new_coord[1] + 1
  if(dir == "s")
    new_coord[1] = new_coord[1] - 1
  # South West and East
  if(dir == "sw"){
    new_coord[2] = new_coord[2] + 1
    if(pos == "upper"){
      pos = "lower"
    }else{
      new_coord[1] = new_coord[1] - 1
      pos = "upper"
    }
  }
  if(dir == "se"){
    new_coord[2] = new_coord[2] - 1
    if(pos == "upper"){
      pos = "lower"
    }else{
      new_coord[1] = new_coord[1] - 1
      pos = "upper"
    }
  }
  # North West and East
  if(dir == "nw"){
    new_coord[2] = new_coord[2] + 1
    if(pos == "upper"){
      pos = "lower"
      new_coord[1] = new_coord[1] + 1
    }else{
      pos = "upper"
    }
  }
  if(dir == "ne"){
    new_coord[2] = new_coord[2] - 1
    if(pos == "upper"){
      pos = "lower"
      new_coord[1] = new_coord[1] + 1
    }else{
      pos = "upper"
    }
  }
  coord = new_coord
  return(list(coord, pos))
}

gotorescue = function(lost_child){
  rescue = list(c(0, 0), "upper")
  step = 0
  while(!all(rescue[[1]] == lost_child[[1]])){
    step = step + 1
    # Go up and down if the column is the same, otherwise go diagonal
    if(rescue[[1]][2] == lost_child[[1]][2]){
      dir = ifelse(rescue[[1]][1] > lost_child[[1]][1], "s", "n")
      rescue = update_pos(c(rescue, dir))
    }else{
      if(rescue[[1]][2] > lost_child[[1]][2]){
        dir = "e"
      }else{
        dir = "w"
      }
      if(rescue[[1]][1] >= lost_child[[1]][1]){
        if(rescue[[1]][1] == lost_child[[1]][1]){
          if(rescue[[1]][2] >= lost_child[[1]][2]){
            dir = paste0("n", dir)
          }else{
            dir = paste0("s", dir)
          }
        }else{
          dir = paste0("s", dir)
        }
      }else{
        dir = paste0("n", dir)
      }
      rescue = update_pos(c(rescue, dir))
    }
  }
  return(step)
}

lost_child = list(c(0,0), "upper")
for(i in input)
  lost_child = update_pos(c(lost_child, i))
gotorescue(lost_child)

###################### PART 2 ######################
lost_child = list(c(0,0), "upper")
distance = NULL
for(i in input){
  lost_child = update_pos(c(lost_child, i))
  distance = c(distance, gotorescue(lost_child))
}
max(distance)
  