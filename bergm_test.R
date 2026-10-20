pacman::p_load(igraph,tidyverse, Bergm, ergm, data.table)

tags_df <- read_csv('dat/anon_tags.csv')

network_df <- tags_df %>%
  inner_join(tags_df, by = "anon_work_id") %>% 
  select(node_a = tags.x, node_b = tags.y, fandom_label = fandom_label.x, date = date.x, anon_work_id) %>% 
  filter(node_a != node_b) %>% 
  igraph::graph_from_data_frame(directed = FALSE) %>% 
  igraph::as_data_frame(what = "edges") %>% 
  distinct() 


nw <- network(network_df, directed = F, multiple = T)


summary(nw~edges)

m1 <- bergm(nw~edges)

###### fandoms #######

idol <- network_df %>% 
  filter(fandom_label=="American Idol") 

lotr <- network_df %>% 
  filter(fandom_label == "LOTR RPF")

hatch <- network_df %>% 
  filter(fandom_label == "Hatchetfield Universe")

