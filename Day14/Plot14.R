library(tidyverse)
library(viridis)
source("Day14.R")

# Reshape the matrix to a melted dataframe
matrix_df = matrix %>%
  data.frame() %>% 
  mutate(Row = row_number()) %>%
  gather(Col, Val, -Row) %>%
  mutate(Col = as.numeric(gsub("X", "", Col))) %>% 
  filter(Val != 0) %>% 
  mutate(Val = factor(Val, levels = sample(unique(Val))))

# Plot
ggplot(matrix_df, aes(x = Row, y = Col, fill = as.factor(Val))) + 
  geom_tile() + 
  scale_fill_viridis(discrete = T, option = "B") + 
  theme_void() + 
  theme(legend.position = "none")
