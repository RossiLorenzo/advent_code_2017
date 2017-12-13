library(stringr)
input = readLines("Day9/Input9_1.txt")

###################### PART 1 ######################
# Pre-cleaning from garbage
input_clean = str_replace_all(input, "\\!\\!", "")
input_clean = str_replace_all(input_clean, "\\!.", "")
input_clean = str_replace_all(input_clean, "\\<[^\\>]*\\>", "")
# Find levels
input_split = strsplit(input_clean, "", fixed = T)[[1]]
level = 0; mysum = 0
for(i in input_split){
  if(i == "{"){
    level = level + 1
  }
  if(i == "}"){
    mysum = mysum + level
    level = level - 1
  }
}
mysum

###################### PART 2 ######################
input_clean = str_replace_all(input, "\\!\\!", "")
input_clean = str_replace_all(input_clean, "\\!.", "")
sum(nchar(str_match_all(input_clean, "\\<([^\\>]*)\\>")[[1]][,2]))
