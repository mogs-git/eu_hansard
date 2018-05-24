library(tidygraph)
library(ggraph)
library(igraph)

# remove speeches that just say "my lords"
appearances_tib_f <- appearances_tib %>% filter(!str_detect(speeches, "My Lords[^, ]"))

# call lords by surname for plotting purposes
surname <- full_speeches$name %>% str_match("^(?:\\w+\\s)(\\w+)") 
surname <- surname[,2]
# Rename duplicated surnames
full_speeches$name[which(surname == "Smith")]
surname[40] <- "Smith_Newnham"
surname_join <- tibble(name = full_speeches$name, surname)
appearances_tib_f %<>% left_join(surname_join) %>% mutate(name = surname) %>% select(-surname)

# Get all pairs of speakers and parties.
# arrange pairs in alphabetical order to be counted.
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
nodes <- appearances_tib_f %>% pull(name) 

g2 <- graph( edges=edgy_lords, directed=F ) 

paired_seq_tib %>% arrange(desc(weights))
pjoin <- surname_join %>% left_join(select(full_speeches, name, party)) %>% select(surname, party)
main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
pjoin %<>% mutate(party = ifelse(party %in% main_parties, party, 'Other'))
paired_party_tib <- paired_seq_tib %>% ungroup() %>% left_join(pjoin, by = c('px' = 'surname')) %>% left_join(pjoin, by = c('py' = 'surname')) %>%
  mutate(px = pmin(party.x, party.y), py = pmax(party.x, party.y)) 

# counts of pairs
paired_party_tib %>% group_by(px, py) %>% summarise(total_interactions = sum(weights)) %>% arrange(desc(total_interactions)) %>% View()
# remember it does not make sense to compare order here (for that look at name/name_shift)
# because lords were sorted alphabetically first.

# Visualise
tidygraph <- as_tbl_graph(g2)
(lord_edges <- edgy_lords[!duplicated(edgy_lords)] %>% as.tibble() %>% mutate(r = row_number()))

# add some extra features to graph data
tidygraph %<>% 
  mutate(centrality = centrality_authority()) %>% 
  mutate(top = centrality > sort(centrality, decreasing = T)[11]) %>%
  left_join(pjoin, by = c('name' = 'surname')) %>%
  mutate(party = as.factor(party)) %>%
  activate(edges) %>%
  left_join(lord_edges, by = c('from' = 'r')) %>%
  left_join(lord_edges, by = c('to' = 'r')) 

# More features to filter by when plotting;
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
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines')) +
  theme_graph()

tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% tory_lords & value.y %in% tory_lords, 1, 0.2)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.15, label.padding = unit(0.1, 'lines')) +
  theme_graph()

tidygraph %>%
  mutate(col_edge = ifelse(value.x %in% tory_lords & value.y %in% tory_lords, "blue", NA)) %>%
  mutate(col_edge = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, "red", col_edge)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(filter(col_edge == "white"), aes(width = weights), colour = "white") +
  scale_colour_manual(values = c("blue", "red", "white")) +
  scale_edge_width(c(0.5,1.5)) +
  geom_node_point(aes(size = centrality)) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.15, label.padding = unit(0.1, 'lines')) +
  theme_graph() 

# Pure tory edges
tidygraph %>%
  mutate(tory_edge = ifelse(value.x %in% tory_lords & value.y %in% tory_lords, TRUE, FALSE)) %>%
  activate(nodes) %>%
  mutate(tory = ifelse(name %in% tory_lords, TRUE, FALSE)) %>%
  activate(edges) %>% 
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(alpha = tory_edge, colour = tory_edge, width = weights)) +
  scale_edge_width(range = c(0.5,1.5)) +
  scale_edge_alpha_manual(values = c(0, 0.5)) +
  scale_edge_colour_manual(values = c("blue", "blue")) +
  geom_node_point(aes(filter = tory, size = centrality)) +
  geom_node_label(aes(filter = tory, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
  theme_graph()

# Pure labour edges
tidygraph %>%
  mutate(labour_edge = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, TRUE, FALSE)) %>%
  activate(nodes) %>%
  mutate(labour = ifelse(name %in% labour_lords, TRUE, FALSE)) %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(aes(alpha = labour_edge, colour = labour_edge, width = weights)) +
  scale_edge_width(range = c(0.5,1.5)) +
  scale_edge_alpha_manual(values = c(0, 0.5)) +
  scale_edge_colour_manual(values = c("red", "red")) +
  geom_node_point(aes(filter = labour, size = centrality)) +
  geom_node_label(aes(filter = labour, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
  theme_graph() 

# Labour + Tory edges

tidygraph %>%
  mutate(edge_col = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, "labour", 
                              ifelse(value.x %in% tory_lords & value.y %in% tory_lords, "tory",
                                     ifelse((value.x %in% tory_lords & value.y %in% labour_lords) | (value.x %in% labour_lords & value.y %in% tory_lords), "mixed", "NA")))) %>%
  activate(nodes) %>%
  mutate(labour = ifelse(name %in% labour_lords | name %in% tory_lords, TRUE, FALSE)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = edge_col, colour = edge_col, width = weights)) +
  scale_edge_width(range = c(0.5,1.5)) +
  scale_edge_alpha_manual(values = c(1, 1,0.5,0)) +
  scale_edge_colour_manual(values = c("red", "blue", "purple", "grey")) +
  geom_node_point(aes(filter = labour, size = centrality)) +
  geom_node_label(aes(filter = labour, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 0.2, label.padding = unit(0.1, 'lines')) +
  theme_graph() 

# Do lords who say the most make the most connections?

# Count number of words in each speech (SUMMARY)
(words_per_speech <- appearances_tib %>% mutate(r = row_number()) %>% unnest_tokens(word, speeches) %>% 
    group_by(r, name) %>% summarise(word_count = n()))

# Count total number of words said by each lord
(words_per_lord <- words_per_speech %>% group_by(name) %>% summarise(word_count = sum(word_count)) %>% filter(!is.na(name)))

words_per_lord %<>% left_join(surname_join) %>% select(-name) %>% rename(name = surname)

tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% topnames | value.y %in% topnames, 1, 0.2)) %>%
  activate(nodes) %>%
  left_join(words_per_lord) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = word_count, colour = party)) +
  geom_node_label(aes(filter = top, label = name, alpha = 0), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines')) +
  theme_graph()
