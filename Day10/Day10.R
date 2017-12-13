input = readLines("Day10/Input10_1.txt")
input_numeric = as.numeric(strsplit(input, ",")[[1]])

###################### PART 1 ######################
mylist = 0:255
skip = 0
pos = 1
for(i in input_numeric){
  mylist[(pos : (pos + i - 1) - 1) %% length(mylist) + 1] = rev(mylist[(pos : (pos + i - 1) - 1) %% length(mylist) + 1])
  pos = pos + i + skip
  skip = skip + 1
}
mylist[1] * mylist[2]

###################### PART 2 ######################
asc = function(x) { strtoi(charToRaw(x), 16L) }
input_numeric = c(unlist(sapply(as.character(input), asc))[,1], 17, 31, 73, 47, 23)
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
