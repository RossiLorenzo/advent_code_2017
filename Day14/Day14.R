library(BMS)
input = readLines("Day14/Input14_1.txt")

###################### PART 1 ######################
# From Day 10 function to calculate the knot hash
knot_hash = function(hash){
  asc = function(x) { strtoi(charToRaw(x), 16L) }
  input_numeric = c(unlist(sapply(as.character(hash), asc))[,1], 17, 31, 73, 47, 23)
  mylist = 0:255
  skip = 0
  pos = 1
  for(j in 1:64){
    for(i in input_numeric){
      mylist[(pos : (pos + i - 1) - 1) %% length(mylist) + 1] = rev(mylist[(pos : (pos + i - 1) - 1) %% length(mylist) + 1])
      pos = pos + i + skip
      skip = skip + 1
    }
  }
  hashed_mylist = tapply(mylist, floor(0:255 / 16), function(x){ Reduce(bitwXor, x)})
  hexadecimal = as.hexmode(hashed_mylist)
  paste0(hexadecimal, collapse = "")
}
part1matrix = sapply(sapply(paste(input, 0:127, sep = "-"), knot_hash), hex2bin)

###################### PART 2 ######################
# Utility function to find left and upper neighbor
find_neighbors = function(i, j){
  if(i == 1)
    if(j != 1)
      return(list(c(i, j - 1)))
    else
      return(NULL)
  return(list(c(i, j - 1), c(i - 1, j)))
}
# Scan the matrix
matrix = t(part1matrix)
counter = 2
for(i in 1:nrow(matrix)){
  for(j in 1:ncol(matrix)){
    neigh = find_neighbors(i, j)
    if(matrix[i, j] == 0){
      counter = counter + 1
      next()
    }
    if(is.null(neigh)){
      matrix[i, j] = counter
      next()
    }
    neigh_value = unlist(lapply(neigh, function(x){ matrix[x[1], x[2]] }))
    if(all(neigh_value == 0)){
      matrix[i, j] = counter
      next()
    }
    if(length(unique(neigh_value[neigh_value != 0])) == 2){
      matrix[matrix == max(neigh_value)] = min(neigh_value)
      matrix[i, j] = min(neigh_value)
      next()
    }
    matrix[i, j] = unique(neigh_value[neigh_value != 0])
  }
  counter = counter + 1
}
length(unique(c(matrix))) - 1
