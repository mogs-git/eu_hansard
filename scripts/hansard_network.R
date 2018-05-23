library(tidygraph)
library(ggraph)
library(igraph)

appearances_tib_f <- appearances_tib %>% filter(!str_detect(speeches, "My Lords[^, ]"))

# call lords by surname for plotting purposes
surname <- full_speeches$name %>% str_match("^(?:\\w+\\s)(\\w+)") 
surname <- surname[,2]
full_speeches$name[which(surname == "Smith")]
surname[40] <- "Smith_Newnham"
surname_join <- tibble(name = full_speeches$name, surname)

appearances_tib_f %<>% left_join(surname_join) %>% mutate(name = surname) %>% select(-surname)

sequence_tib <- appearances_tib_f %>% select(-speeches)
sequence_tib %<>% mutate(name_shift = lead(name), party_shift = lead(party))
sequence_tib %<>% mutate(px = pmin(name, name_shift), py = pmax(name, name_shift))

paired_seq_tib <-  sequence_tib %>% 
  group_by(px, py) %>%
  summarise(weights = n()) %>% filter(!is.na(px))

# convert this into a vector where each pair follows consequetively
edgy_lords <- list()
l1 <- paired_seq_tib$px
l2 <- paired_seq_tib$py
for (i in 1:(length(l1))) {
  edgy_lords[[i]] <- c(l1[i], l2[i])
}
edgy_lords %<>% unlist()

# get speaker names as distinct nodes
nodes <- appearances_tib_f %>% pull(name) %>% unique() %>% length()

g2 <- graph( edges=edgy_lords, n=nodes, directed=F ) 

paired_seq_tib %>% arrange(desc(weights))
pjoin <-surname_join %>% left_join(select(full_speeches, name, party)) %>% select(surname, party)
main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
pjoin %<>% mutate(party = ifelse(party %in% main_parties, party, 'Other'))
paired_party_tib <- paired_seq_tib %>% ungroup() %>% left_join(pjoin, by = c('px' = 'surname')) %>% left_join(pjoin, by = c('py' = 'surname')) %>%
  mutate(px = pmin(party.x, party.y), py = pmax(party.x, party.y)) 

# px and py are sorted orders, party.x and party.y are unsorted (order as appears)
paired_party_tib %>% group_by(px, py) %>% summarise(total_interactions = sum(weights)) %>% arrange(desc(total_interactions)) %>% View()

paired_party_tib %>% group_by(party.x, party.y) %>% summarise(total_interactions_ordered = sum(weights)) %>% arrange(desc(total_interactions_ordered)) %>% View()



tidygraph <- as_tbl_graph(g2)
lord_edges <- edgy_lords[!duplicated(edgy_lords)] %>% as.tibble() %>% mutate(r = row_number())
lord_edges
tidygraph %>%
  activate(edges) %>%
  left_join(lord_edges, by = c('from' = 'r')) %>%
  left_join(lord_edges, by = c('to' = 'r'))

tidygraph %<>% 
  mutate(centrality = centrality_authority()) %>% 
  mutate(top = centrality > sort(centrality, decreasing = T)[11]) %>%
  left_join(pjoin, by = c('name' = 'surname')) %>%
  mutate(party = as.factor(party)) %>%
  activate(edges) %>%
  left_join(lord_edges, by = c('from' = 'r')) %>%
  left_join(lord_edges, by = c('to' = 'r')) 

topnames <- tidygraph %>% activate(nodes) %>% filter(top) %>% pull(name)
tory_lords <- tidygraph  %>% activate(nodes) %>% filter(party == "(Con)") %>% pull(name)
labour_lords <-  tidygraph  %>% activate(nodes) %>% filter(party == "(Lab)") %>% pull(name)
tidygraph %<>% activate(edges) %>% mutate(weights = paired_seq_tib$weights)

tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% topnames | value.y %in% topnames, 1, 0.2)) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.15, label.padding = unit(0.1, 'lines')) +
  theme_graph()

tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% tory_lords & value.y %in% tory_lords, 1, 0.2)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  geom_node_point(aes(size = centrality, colour = party)) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.15, label.padding = unit(0.1, 'lines')) +
  theme_graph()

tidygraph %>%
  mutate(col_edge = ifelse(value.x %in% tory_lords & value.y %in% tory_lords, "blue", "white"),
         col_edge = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, "red", col_edge)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(filter(col_edge == "white"), aes(width = weights), colour = "white") +
  scale_colour_manual(values = c("blue", "red", "white")) +
  scale_edge_width(c(0.5,1.5)) +
  geom_node_point(aes(size = centrality)) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.15, label.padding = unit(0.1, 'lines')) +
  theme_graph() 

tidygraph %>%
  mutate(tory_edge = ifelse(value.x %in% tory_lords & value.y %in% tory_lords, TRUE, FALSE)) %>%
  activate(nodes) %>%
  mutate(tory = ifelse(name %in% tory_lords, TRUE, FALSE)) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(filter = tory_edge, width = weights), colour = "blue") +
  scale_edge_width(c(0.5,1.5)) +
  geom_node_point(aes(filter = tory, size = centrality)) +
  geom_node_label(aes(filter = tory, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
  theme_graph()

tidygraph %>%
  mutate(labour_edge = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, TRUE, FALSE)) %>%
  activate(nodes) %>%
  mutate(labour = ifelse(name %in% labour_lords, TRUE, FALSE)) %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(filter = labour_edge, width = weights), colour = "red") +
  scale_edge_width(c(0.5,1.5)) +
  geom_node_point(aes(filter = labour, size = centrality)) +
  geom_node_label(aes(filter = labour, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
  theme_graph() 
