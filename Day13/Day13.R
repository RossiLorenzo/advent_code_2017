input = readLines("Day13/Input13_1.txt")
input_df = data.frame(
  Depth = as.numeric(gsub(":.*", "", input)),
  Range = as.numeric(gsub(".* ", "", input)),
  Pos = 1, 
  Dir = "+",
  stringsAsFactors = F
)
###################### PART 1 ######################
layer_index = 0
score = 0
while(layer_index <= max(input_df$Depth)){
  firewall_base = input_df[input_df$Depth == layer_index, ]
  if(nrow(firewall_base) != 0)
    if(firewall_base$Pos == 1)
      score = score + layer_index * firewall_base$Range
  # Update firewall
  input_df$Pos[input_df$Dir == "+"] = input_df$Pos[input_df$Dir == "+"] + 1
  input_df$Pos[input_df$Dir == "-"] = input_df$Pos[input_df$Dir == "-"] - 1
  input_df$Dir[input_df$Pos == input_df$Range] = "-"
  input_df$Dir[input_df$Pos == 1] = "+"
  # Update position
  layer_index = layer_index + 1
}
score


###################### PART 2 ######################

