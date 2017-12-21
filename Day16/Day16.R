# Read input and make it a dataframe
input = strsplit(strsplit(readLines("Day16/Input16_1.txt"), ",", fixed = T)[[1]], "", fixed = T)
input_df = do.call(rbind, lapply(input, function(x){
  data.frame(
    Dance = ifelse(x[1] == "s", "spin", ifelse(x[1] == "x", "exchange", "partner")),
    Type = paste0(x[-1], collapse = ""),
    stringsAsFactors = F
  )
}))
dance_type = input_df$Dance
split_type = strsplit(input_df$Type, "/", fixed = T)
arg1 = sapply(split_type, '[[', 1)
arg2 = sapply(split_type, function(x){ 
  tmp = try(x[[2]], silent = T)
  if('try-error' %in% class(tmp))
    return(NA)
  tmp
})

###################### PART 1 ######################
# Dance moves
spin = function(order, arg1){
  arg1 = as.numeric(arg1)
  order[c((length(order) - (arg1 - 1)) : length(order), 1 : (length(order) - (arg1)))]
}
exchange = function(order, arg1, arg2){
  arg1 = as.numeric(arg1) + 1; arg2 = as.numeric(arg2) + 1
  tmp = order[as.numeric(arg1)]
  order[arg1] = order[arg2]
  order[arg2] = tmp
  return(order)
}
partner = function(order, arg1, arg2){
  pos = which(order %in% c(arg1, arg2)) - 1
  exchange(order, pos[1], pos[2])
}

# Run dance
program_order = letters[1:16]
for(i in 1:nrow(input_df)){
  if(dance_type[i] == "spin"){
    program_order = spin(program_order, arg1[i])
  }else{
    if(dance_type[i] == "exchange"){
      program_order = exchange(program_order, arg1[i], arg2[i])  
    }else{
      program_order = partner(program_order, arg1[i], arg2[i])  
    }
  }
}
paste0(program_order, collapse = "")

###################### PART 2 ######################
program_order = letters[1:16]
seen_orders = paste0(program_order, collapse = "")
for(j in 1:10^9){
  print(j)
  for(i in 1:nrow(input_df)){
    if(dance_type[i] == "spin"){
      program_order = spin(program_order, arg1[i])
    }else{
      if(dance_type[i] == "exchange"){
        program_order = exchange(program_order, arg1[i], arg2[i])  
      }else{
        program_order = partner(program_order, arg1[i], arg2[i])  
      }
    }
  }
  if(paste0(program_order, collapse = "") %in% seen_orders)
    break()
  seen_orders = c(seen_orders, paste0(program_order, collapse = ""))
}
paste("After only", j, "moves the dance is back on the original position.")
paste("So we can get the solution simply running 10^9 %%", j, "=", 10^9 %% j, "iterations")
seen_orders[(10^9 %% j) + 1]