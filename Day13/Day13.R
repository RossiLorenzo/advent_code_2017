input = readLines("Day13/Input13_1.txt")
dept = as.numeric(gsub(":.*", "", input))
range = as.numeric(gsub(".* ", "", input))
pos = rep(1, length(range))
dir = rep("+", length(range))
max_dept = max(dept)

###################### PART 1 ######################
run_through = function(dept, range, pos, dir, max_dept, type){
  layer_index = 0
  score = 0
  while(layer_index <= max_dept){
    firewall_base = which(dept == layer_index)
    if(length(firewall_base) != 0)
      if(pos[firewall_base] == 1){
        score = score + layer_index * range[firewall_base]
        if(type == "caught")
          return(TRUE)
      }
    # Update firewall
    positives = dir == "+"
    negatives = dir == "-"
    pos[positives] = pos[positives] + 1
    pos[negatives] = pos[negatives] - 1
    max_range = pos == range
    dir[max_range] = "-"
    min_range = pos == 1
    dir[min_range] = "+"
    # Update position
    layer_index = layer_index + 1
  }
  if(type == "caught")
    return(FALSE)
  score
}
run_through(dept, range, pos, dir, max_dept, "score")

###################### PART 2 ######################
caught = TRUE
wait = 0
while(caught){
  # Update firewall
  positives = dir == "+"
  negatives = dir == "-"
  pos[positives] = pos[positives] + 1
  pos[negatives] = pos[negatives] - 1
  max_range = pos == range
  dir[max_range] = "-"
  min_range = pos == 1
  dir[min_range] = "+"
  # Run Through
  caught = run_through(dept, range, pos, dir, max_dept, "caught")
  wait = wait + 1
}
wait
