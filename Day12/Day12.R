input = readLines("Day12/Input12_1.txt")
input_df = strsplit(input, " <-> ", fixed = T)
input_df = lapply(input_df, function(x){
  data.frame(From = x[1], To = strsplit(gsub(" ", "", x[2]), ",", fixed = T)[[1]], stringsAsFactors = F)
})
input_df = do.call(rbind, input_df)

###################### PART 1 ######################
find_connections = function(df, node){
  unique(c(df$From[df$To %in% node], df$To[df$From %in% node]))
}
connections = NULL
new_connections = unique(find_connections(input_df, "0"))
while(length(setdiff(new_connections, connections)) != 0){
  connections = unique(c(connections, new_connections))
  new_connections = unique(find_connections(input_df, connections))
}
length(connections)

###################### PART 2 ######################
groups = 1
filter_df = input_df[!(input_df$From %in% connections | input_df$To %in% connections), ]
while(nrow(filter_df) != 0){
  connections = NULL
  new_connections = unique(find_connections(filter_df, filter_df$From[1]))
  while(length(setdiff(new_connections, connections)) != 0){
    connections = unique(c(connections, new_connections))
    new_connections = unique(find_connections(filter_df, connections))
  }
  filter_df = filter_df[!(filter_df$From %in% connections | filter_df$To %in% connections), ]
  groups = groups + 1
}
groups
