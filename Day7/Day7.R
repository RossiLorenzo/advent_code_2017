library(tidyverse)
input = readLines("Day7/Input7_1.txt")

###################### PART 1 ######################
input_df = data_frame(
  Text = input, 
  Node = gsub(" .*","", Text),
  Child = strsplit(ifelse(grepl("->", Text), gsub(".*-> ", "", Text), NA), ","),
  Weight = as.numeric(gsub(".*\\(|\\).*", "", Text))
) %>% 
  unnest(Child) %>% 
  mutate(Child = str_trim(Child)) 
input_df = input_df %>% 
  left_join(input_df %>% select(Dad = Node, Child), by = c("Node" = "Child"))

(res = filter(input_df, is.na(Dad)) %>% select(Node) %>% distinct())

###################### PART 2 ######################
# Function to calculate a node and children weight
children_weight = function(node){
  # Find all children
  all_children = input_df %>% 
    filter(Node == node)
  children = unlist(all_children$Child)
  if(all(is.na(children)))
    return(unique(all_children$Weight))
  return(unique(all_children$Weight) + sum(sapply(children, children_weight)))
}
# Function to go up in the tree and check balances
all_children = input_df %>% filter(Node == unlist(res))
all_weights = sapply(all_children$Child, children_weight)
prev_all_weights = all_weights
while(length(unique(all_weights)) != 1){
  unb_node = names(all_weights[all_weights == names(table(all_weights))[2]])
  all_children = input_df %>% filter(Node == unlist(unb_node))
  prev_all_weights = all_weights
  all_weights = sapply(all_children$Child, children_weight)
}
unique(input_df$Weight[input_df$Node == unlist(unb_node)]) - max(abs(diff(prev_all_weights)))
