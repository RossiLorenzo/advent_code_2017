input = as.numeric(readLines("Day3/Input3_1.txt"))

###################### PART 1 ######################
# Every spiral has a side of length 1 + (2 * i) so the perimeter of the i-th square is:
  # 2 * (1 + (2 * i)) + 2 * (1 + (2 * (i - 1))) =  8 * i
# This means that to calculate on which spiral any element is we can use:
add_spiral = function(i){ 1 + sum(1 + 8 * 0:i)}
which_spiral = function(x){
  it = 1
  while(add_spiral(it) <= x) 
    it = it + 1
  it
}

# Once we know which spiral the element is on, we can also find the position in such spiral
which_position = function(x){
  x - add_spiral(which_spiral(x) - 1)
}

# Knowing which spiral we are on, the spiral length and the position we can calculate the coordinates
which_coordinates = function(x){
  if(x == 1)
    return(c(0, 0))
  spiral = which_spiral(x)
  position = which_position(x)
  # Same side
  if(position <= spiral)
    return(c(spiral, position))
  # Ceiling
  if(position <= (spiral * 3))
    return(c(spiral - (position - spiral), spiral))
  # Opposite side
  if(position <= (spiral * 5))
    return(c(-spiral, spiral - (position - spiral * 3)))
  # Floor
  if(position <= (spiral * 7))
    return(c((position - spiral * 5) - spiral, -spiral))
  # Same side but down
  return(c(spiral + 1, x - add_spiral(spiral)))
}

# Finally I can calculate the coordinates of the input and sum their abs to find distance
sum(abs(which_coordinates(input)))

###################### PART 2 ######################
# Build the spyral step by step
data = data.frame(Index = 1, PosX = 0, PosY = 0, Value = 1)
while(max(data$Value) <= input){
  Pos = which_coordinates(max(data$Index) + 1)
  distances = apply(data, 1, function(x){ floor(dist(matrix(c(x[2], x[3], Pos), byrow = T, ncol = 2))) })
  neigh_data = data[distances <= 1, ]
  data = rbind(data, data.frame(Index = max(data$Index) + 1, PosX = Pos[1], PosY = Pos[2], Value = sum(neigh_data$Value)))
}
