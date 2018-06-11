
# call lords by surname for plotting purposes
appearances_tib_fp

# Get all pairs of speakers and parties.
# arrange pairs in alphabetical order to be counted.
sequence_tib <- appearances_tib_fp %>% select(surname, party)
sequence_tib %<>% mutate(name_shift = lead(surname), party_shift = lead(party))
sequence_tib %<>% mutate(px = pmin(surname, name_shift), py = pmax(surname, name_shift))

# Every pair and how many times they appear
paired_seq_tib <-  sequence_tib %>% 
  group_by(px, py) %>%
  summarise(weights = n()) %>% filter(!is.na(px))

paired_seq_tib %>% arrange(desc(weights)) # Most talkative pairs

# The same but minus the chief whip
paired_seq_noTaylor <- sequence_tib %>% filter(px != "Taylor", py != "Taylor")

# Numerical features of the network

paired_seq_tib %>% ungroup() %>% summarise(sum(weights))
# 220 pairs

paired_seq_tib %>% ungroup() %>% summarise(nrow(.))
# make 155 unique connections (310 speakers involved)

# counts of connections
c1 <- paired_seq_tib %>% ungroup() %>% select(px, weights)
c2 <- paired_seq_tib %>% ungroup() %>% select(py, weights) %>% rename(px = py)
total_connections <- c1 %>% bind_rows(c2) %>% group_by(px) %>% summarise(s = sum(weights))

# n unique connections
c3 <- paired_seq_tib %>% count(px)
c4 <- paired_seq_tib %>% count(py) %>% rename(px = py)
unique_connections <- c3 %>% bind_rows(c4) %>% group_by(px) %>% summarise(su = sum(n))

total_connections %<>% left_join(unique_connections) %>% group_by(px) %>% mutate(sr = su/s) %>% arrange(sr)
total_connections %<>% arrange(desc(su)) %>% left_join(party_id_tib, by = c("px" = "surname"))

# For speakers making more than 2 connections, which party has the highest unique:total ratio?
total_connections %>% filter(s > 2) %>% ggplot(aes(sr, colour = party_f)) + geom_freqpoly(size = 1)
total_connections %>% filter(s > 2) %>% group_by(party_f) %>% summarise(median_s = median(s), median_u = median(su), median_su = median_u/median_s)
total_connections %>% filter(s > 2) %>% ggplot(aes(su/s))+ geom_histogram() + facet_wrap(~party_f) + coord_cartesian(xlim = c(0,1))

total_connections %<>% arrange(desc(sr)) %>% filter(s > 2) %>% add_gender()
total_connections %>% group_by(gender) %>% summarise(median(sr))

total_connections %<>% left_join(words_per_lord)
words_per_lord_per_speech <- words_per_lord %>% left_join(speech_counts) %>% mutate(p = word_count/n)
total_connections %>%
  group_by(sr) %>%
  summarise(m = mean(word_count, na.rm = T), s = sd(word_count, na.rm = T), upper = m+s, lower = m-s) %>%
  print(.) %>%
  ggplot(aes(sr, m)) + geom_point()

total_connections %>% 
  left_join(words_per_lord_per_speech) %>%
  ggplot(aes(sr, p)) + geom_point(alpha = 0.2)
  

(words_per_speech <- appearances_tib %>% mutate(r = row_number()) %>% unnest_tokens(word, speeches) %>% 
    group_by(r, name) %>% summarise(word_count = n()))

# Count total number of words said by each lord
(words_per_lord <- words_per_speech %>% group_by(name) %>% summarise(word_count = sum(word_count)) %>% filter(!is.na(name)))

words_per_lord %<>% add_surname()

speech_counts <- appearances_tib_fp %>% group_by(surname) %>% count() %>% arrange(desc(n)) 
speech_counts %>% ggplot(aes(n)) + geom_histogram(bins = 12) + scale_x_continuous(breaks = 1:12)
quantile(speech_counts$n, 0.9) 
ecdf(speech_counts$n)(3)
ecdf(speech_counts$n)(1)
median(speech_counts$n)

# Convert each pair to thier respective parties
pjoin <- party_id_tib %>% select(-name, -party) %>% mutate(party = as.character(party_f)) %>% select(-party_f)
paired_party_tib <- paired_seq_tib %>% ungroup() %>% left_join(pjoin, by = c('px' = 'surname')) %>% left_join(pjoin, by = c('py' = 'surname')) %>%
  mutate(px = pmin(party.x, party.y), py = pmax(party.x, party.y)) 

# counts of pairs
paired_party_tib %>% group_by(px, py) %>% summarise(total_interactions = sum(weights)) %>% arrange(desc(total_interactions))
# remember it does not make sense to compare order here (for that look at name/name_shift)
# because lords were sorted alphabetically first.

diffPartyInteractions <- paired_seq_tib %>% 
  bind_cols(select(paired_party_tib, party.x, party.y)) %>%
  mutate(pdiff = party.x != party.y) 
  
party_interactions <- tibble(name = c(diffPartyInteractions$px, diffPartyInteractions$py), diffParty = rep(diffPartyInteractions$pdiff,2)) %>%
  group_by(name) %>% summarise(n_same_party = sum(!diffParty), n_diff_party = sum(diffParty)) %>% arrange(desc(n_same_party))

party_interactions %>% ggplot(aes(n_same_party, n_diff_party)) + geom_point()

library(WVPlots)
WVPlots::ScatterHist(party_interactions, "n_same_party", "n_diff_party", 
                     title="Boyo")

# Visualise

# First construct the graph

# EDGES
# convert this into a vector where each pair follows consequetively
edgy_lords <- list()
l1 <- paired_seq_tib$px
l2 <- paired_seq_tib$py
for (i in 1:(length(l1))) {
  edgy_lords[[i]] <- c(l1[i], l2[i])
}
edgy_lords %<>% unlist()

# NODES
# get speaker names as distinct nodes
nodes <- appearances_tib_fp %>% pull(surname) 

# Make graph object
g2 <- graph( edges=edgy_lords, directed=F ) 


tidygraph <- as_tbl_graph(g2)
(lord_edges <- edgy_lords[!duplicated(edgy_lords)] %>% as.tibble() %>% mutate(r = row_number()))

pjoin <- party_id_tib %>% select(-party, -name) %>% rename(party = party_f, name = surname)

# add some extra features to graph data
tidygraph %<>% 
  activate(nodes) %>%
  mutate(centrality = centrality_authority()) %>% 
  mutate(top = centrality > sort(centrality, decreasing = T)[11]) %>%
  left_join(pjoin) %>%
  mutate(party = as.factor(party)) %>%
  activate(edges) %>%
  left_join(lord_edges, by = c('from' = 'r')) %>%
  left_join(lord_edges, by = c('to' = 'r')) 

# More features to filter by when plotting;
topnames <- tidygraph %>% activate(nodes) %>% filter(top) %>% pull(name)
tory_lords <- tidygraph  %>% activate(nodes) %>% filter(party == "(Con)") %>% pull(name)
labour_lords <-  tidygraph  %>% activate(nodes) %>% filter(party == "(Lab)") %>% pull(name)
libdem_lords <-  tidygraph  %>% activate(nodes) %>% filter(party == "(LD)") %>% pull(name)
cb_lords <-  tidygraph  %>% activate(nodes) %>% filter(party == "(CB)") %>% pull(name)

tidygraph %<>% activate(edges) %>% mutate(weights = paired_seq_tib$weights)

# Main centrality plot
tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% topnames | value.y %in% topnames, 1, 0.2)) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  scale_colour_manual(values = party_cols) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines')) +
  theme_graph() +
  theme(legend.position="none")

# interactions of one party
tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% tory_lords | value.y %in% tory_lords, 1, 0.2)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  scale_colour_manual(values = party_cols) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.15, label.padding = unit(0.1, 'lines')) +
  theme_graph()

tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% cb_lords | value.y %in% cb_lords, 1, 0.2)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  scale_colour_manual(values = party_cols) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.15, label.padding = unit(0.1, 'lines')) +
  theme_graph()

# Pure party edges
pure_party_edge_plot <- function(tidygraph, lords, clrs) {
  tidygraph %>%
    mutate(group_edge = ifelse(value.x %in% lords & value.y %in% lords, TRUE, FALSE)) %>%
    activate(nodes) %>%
    mutate(lord_group = ifelse(name %in% lords, TRUE, FALSE)) %>%
    activate(edges) %>% 
    ggraph(layout = 'dh') + 
    geom_edge_link(aes(alpha = group_edge, colour = group_edge, width = weights)) +
    scale_edge_width(range = c(0.5,2)) +
    scale_edge_alpha_manual(values = c(0, 1)) +
    scale_edge_colour_manual(values = clrs) +
    geom_node_point(aes(filter = lord_group, size = centrality)) +
    #geom_node_label(aes(filter = lord_group, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
    theme_graph() + theme(legend.position="none")
}
pure_party_edge_plot(tidygraph, tory_lords, c("blue", "blue"))
ggpubr::ggarrange(pure_party_edge_plot(tidygraph, tory_lords, c(party_cols[1], party_cols[1])),
                  pure_party_edge_plot(tidygraph, labour_lords, c(party_cols[4], party_cols[4])),
                  pure_party_edge_plot(tidygraph, libdem_lords, c(party_cols[3], party_cols[3])),
                  pure_party_edge_plot(tidygraph, cb_lords, c(party_cols[2], party_cols[2])))
                  

# single speaker edges
ss_edge_plot <- function(tidygraph, speaker, node_var = NA, coords = NA) {
  tidygraph %<>%
    mutate(group_edge = ifelse(value.x %in% speaker | value.y %in% speaker, TRUE, FALSE)) %>%
    activate(nodes) %>%
    mutate(lord_group = ifelse(name %in% speaker, TRUE, FALSE)) %>%
    mutate(t4 = ifelse(name %in% node_var, TRUE, FALSE))
    #{ifelse (!is.na(node_var), mutate(t4 = ifelse(name %in% node_var, TRUE, FALSE)), .)}
    
  
  speaker_connections <- tidygraph %>% activate(edges) %>% filter(group_edge) %>% select(value.x, value.y) %>% E() %>% attr(which = "vnames") %>% str_split("\\|") %>% unlist() %>% unique()
  tidygraph %>% 
    mutate(speaker_cons = ifelse(name %in% speaker_connections, TRUE, FALSE)) %>%
    ggraph(layout = "manual", circular = FALSE, node.positions = coords) + 
    geom_edge_link(aes(alpha = group_edge, width = weights)) +
    scale_edge_width(range = c(0.5,1.5)) +
    scale_edge_alpha_manual(values = c(0.1, 0.8)) +
    scale_edge_colour_manual(values = clrs) +
    geom_node_point(aes(filter = speaker_cons, colour = party, shape = lord_group, size = centrality)) +
    #geom_node_label(aes(filter = speaker_cons, label = name, alpha = 0, size = 0.02), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
    theme_graph() + theme(legend.position="none") + ggtitle(speaker)
}

coords <- create_layout(tidygraph, "dh") %>% select(x, y)

speech_counts %>% head(10)

ss_edge_plot(tidygraph, "Lea", coords = coords)

top4 <- speech_counts %>% head(4) %>% pull(surname)
top4_networks <- map(top4, ~ss_edge_plot(tidygraph, ., top4, coords))
ggpubr::ggarrange(top4_networks[[1]], top4_networks[[2]], top4_networks[[3]], top4_networks[[4]],
                  nrow = 2, ncol = 2)


# speakers not connected to top 10 most central nodes

tidygraph %>% activate(nodes) %>% pull(centrality) %>% hist(breaks =50)
noncentral_lordsx <- tidygraph %>%
  filter(value.x %!in% topnames & value.y %!in% topnames) %>%
  pull(value.x) 

noncentral_lordsy <- tidygraph %>%
  filter(value.x %!in% topnames & value.y %!in% topnames) %>%
  pull(value.y) 

noncentral_lords <- unique(c(noncentral_lordsx, noncentral_lordsy))


tidygraph %>%
  mutate(top_edge = ifelse(value.x %!in% topnames & value.y %!in% topnames, 1, 0.2)) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  scale_colour_manual(values = party_cols) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines')) +
  theme_graph() +
  theme(legend.position="none")

full_speeches %<>% add_surname()

full_speeches %>% left_join(speech_counts)
words_per_lord %>% 
  add_surname() %>% 
  left_join(speech_counts) %>%
  mutate(avg_words = word_count/n) %>%
  ggplot(aes(avg_words)) + geom_histogram() + facet_wrap(~n)

words_per_lord %>% 
  add_surname() %>% 
  left_join(speech_counts) %>%
  mutate(avg_words = word_count/n) %>%
  group_by(n) %>% summarise(avg_words_per_n = mean(avg_words), n2 = n()) %>%
  ggplot(aes(n, avg_words_per_n)) + geom_col(aes(fill = n2))


appearances_tib_f %>% filter(str_detect(name, "Keen")) %>% View()


# Labour + Tory edges

tidygraph %>%
  mutate(edge_col = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, "labour", 
                              ifelse(value.x %in% tory_lords & value.y %in% tory_lords, "tory",
                                     ifelse((value.x %in% tory_lords & value.y %in% labour_lords) | (value.x %in% labour_lords & value.y %in% tory_lords), "mixed", "NA")))) %>%
  activate(nodes) %>%
  mutate(labourortory = ifelse(name %in% labour_lords | name %in% tory_lords, TRUE, FALSE),
         labour = ifelse(name %in% labour_lords, TRUE, FALSE)) %>%
  ggraph(layout = 'graphopt') + 
  geom_edge_link(aes(filter = edge_col == "labour"|edge_col == "tory"|edge_col == "mixed", alpha = edge_col, colour = edge_col, width = weights)) +
  scale_edge_width(range = c(0.5,1.5)) +
  scale_edge_alpha_manual(values = c(1, 0.8,1,0)) +
  scale_edge_colour_manual(values = c("red", "purple", "blue", "grey")) +
  geom_node_point(aes(filter = labourortory, size = centrality, colour = labour)) +
  scale_colour_manual(values = c("blue", "red")) +
  geom_node_label(aes(filter = labourortory, label = name, alpha = 0, size = 0.2, hjust = 0, vjust = 0.1), label.padding = unit(0.1, 'lines')) +
  theme_graph() 

# finging community structure: Random walks ----

subgrps <- cluster_walktrap(tidygraph)
subgrps <- cluster_spinglass(tidygraph)
subgrps <- cluster_edge_betweenness(tidygraph)
members <- membership(subgrps)
members_tibble <- tibble(name = names(members), grp = as.factor(members))
tidygraph_wcom <- tidygraph %<>% activate(nodes) %>% left_join(members_tibble)
tidygraph_wcom %<>% activate(edges)

tidygraph_wcom %>%
  mutate(top_edge = ifelse(value.x %in% topnames | value.y %in% topnames, 1, 0.2)) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = grp)) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines')) +
  theme_graph() +
  theme(legend.position="none")


# Nodes connected to tories ----

pullm <- function(df, vars) {
  outlist <- list()
  for(i in seq_along(vars)) {
    outlist[[i]] <- pull(df, vars[[i]])
  }
  outlist
}

tory_connections <- tidygraph %>% filter(value.x %in% tory_lords |value.y %in% tory_lords) %>% pullm(c("value.x", "value.y"))

tory_con_tib <- tibble(px = tory_connections[[1]], py = tory_connections[[2]])

torycons <- appearances_tib_fp %>%
  mutate(name_shift = lead(surname), party_shift = lead(party), next_speech = lead(speeches)) %>%
  mutate(px = pmin(surname, name_shift), py = pmax(surname, name_shift))  %>%
  semi_join(tory_con_tib)

speech1 <- torycons$speeches[torycons$party != "(Con)"]
speech2 <- torycons$next_speech[torycons$party_shift != "(Con)"]

# all speeches connected to tories by speakers from other parties
tory_connecting_speeches <- tibble(speeches = c(speech1, speech2))

bing <- get_sentiments("bing")

appearances_tib_fp %>% 
  mutate(speech_id = row_number()) %>%
  unnest_tokens(word, speeches) %>%
  inner_join(bing) %>%
  filter(word %!in% c("noble", "lord")) %>%
  group_by(speech_id, sentiment) %>% 
  count() %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  ggplot(aes(speech_id, n)) + geom_col(aes(fill = sentiment))

appearances_tib_fp %>% 
  mutate(speech_id = row_number()) %>%
  unnest_tokens(word, speeches) %>%
  inner_join(bing) %>%
  filter(word %!in% c("noble", "lord")) %>%
  group_by(speech_id, sentiment) %>% 
  count() %>%
  spread(sentiment, n) %>%
  mutate(score = positive - negative) %>%
  ggplot(aes(speech_id, score)) + geom_col()

tory_connecting_speeches %>%
  mutate(r = as.factor(row_number())) %>%
  unnest_tokens(word, speeches) %>%
  inner_join(bing) %>%
  filter(word %!in% c("noble", "lord")) %>%
  group_by(r, sentiment) %>% 
  count() %>%
  spread(sentiment, n) %>%
  mutate(score = positive - negative) %>%
  ggplot(aes(fct_reorder(r, score), score)) + geom_col()


library(networkD3)

labTor_graph <- tidygraph %>%
  mutate(edge_col = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, "labour", 
                           ifelse(value.x %in% tory_lords & value.y %in% tory_lords, "tory",
                                  ifelse((value.x %in% tory_lords & value.y %in% labour_lords) | (value.x %in% labour_lords & value.y %in% tory_lords), "mixed", "NA")))) %>%
  activate(nodes) %>%
  mutate(labourortory = ifelse(name %in% labour_lords | name %in% tory_lords, TRUE, FALSE),
         labour = ifelse(name %in% labour_lords, TRUE, FALSE))


labTor_graph
nodes <- attr(V(labTor_graph), which = "names") %>% as.tibble() %>% mutate(id = row_number())
nodes_d3 <- mutate(nodes, id = id - 1)

a <- labTor_graph %>% activate(edges) %>% filter(!str_detect(edge_col, "NA")) %>% pull(value.x)
b <-  labTor_graph %>% activate(edges) %>% filter(!str_detect(edge_col, "NA")) %>% pull(value.y)
edgecols <- labTor_graph %>% activate(edges) %>% filter(!str_detect(edge_col, "NA")) %>% pull(edge_col)
edges <- tibble(a, b)
edges_num <- edges %>% 
  left_join(nodes, by = c("a" = "value")) %>% 
  rename(from = id)

edges_num <- edges_num %>% 
  left_join(nodes, by = c("b" = "value")) %>% 
  rename(to = id)

edges_d3 <- mutate(edges_num, from = from - 1, to = to - 1)
nodes_d3 %<>% left_join(pjoin, by = c("value" = "name"))
edges_d3 %<>% add_column(edgecols)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "value", Group = "party", linkColour = edgecols,
             opacity = 1, fontSize = 16)

# implement in visnetwork ----
# https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
library(visNetwork)

# Nodes and edges
edges <- edgy_lords
edges <- data.frame(from = edges[(1:length(edges) %% 2 ) != 0], to = edges[(1:length(edges) %% 2 ) == 0])
nodes <- data.frame(nodes)

#convert to numeric
nodes %<>% mutate(r = row_number())
edges %<>% left_join(nodes, by = c("from" = "nodes")) %>% left_join(nodes, by = c("to"="nodes"))
edges %<>% rename(value.x = from, value.y = to, from = r.x, to = r.y)
nodes %<>% rename(id = r)

visNetwork(nodes, edges, width = "100%")

visNetwork(nodes, edges, width = "100%")

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

# Simpler features that inform our understanding of the network

# Which lords spoke most often?
speech_counts <- appearances_tib_f %>% group_by(surname) %>% count() %>% arrange(desc(n)) 
speech_counts %>% ggplot(aes(n)) + geom_histogram(bins = 12) + scale_x_continuous(breaks = 1:12)
quantile(speech_counts$n, 0.9) 
ecdf(speech_counts$n)(3)
ecdf(speech_counts$n)(1)
median(speech_counts$n)
party_id_tib <- readRDS("data//party_id_tib.RDAT") %>% select(-name) %>% rename(name = surname)
speech_counts %<>% left_join(party_id_tib)

appearances_tib_f %>% filter(name == "Howard") %>% View()
words_per_speech %>% filter(str_detect(name, "Howard"))

appearances_tib_f %>% filter(name == "Taylor") %>% View()
words_per_speech %>% filter(str_detect(name, "Taylor"))


# testing network d3
# https://christophergandrud.github.io/networkD3/#simple
install.packages("networkD3")

library(networkD3)
subgrps <- cluster_walktrap(tidygraph)
members <- membership(subgrps)

d3_tidygraph <- networkD3::igraph_to_networkD3(tidygraph, group = members)

# Create force directed network plot
networkD3::forceNetwork(Links = d3_tidygraph$links, Nodes = d3_tidygraph$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', opacity = 1)


pp_edge_graph <- pure_party_edge_plot <- function(tidygraph, lords, clrs) {
  tidygraph %>%
    mutate(group_edge = ifelse(value.x %in% lords & value.y %in% lords, TRUE, FALSE)) %>%
    activate(nodes) %>%
    mutate(lord_group = ifelse(name %in% lords, TRUE, FALSE)) %>%
    filter(lord_group)
}
tory_force_graph <- pp_edge_graph(tidygraph, tory_lords, c(mycols[1], mycols[1]))
subgrps <- cluster_walktrap(tory_force_graph)
members <- membership(subgrps)
tory_force_graph <- networkD3::igraph_to_networkD3(tory_force_graph, group = members)
networkD3::forceNetwork(Links = tory_force_graph$links, Nodes = tory_force_graph$nodes,
                        Source = 'source', Target = 'target', 
                        NodeID = 'name', Group = 'group', opacity = 1, fontSize = 20)
                                        
# That was way too easy.... how hard is this going to come back and bite me in the arse?

# let's just, out of interest, see if Taylor breaks in tories more often than other parties

sequence_tib %>% filter(name == "Taylor") %>% count(party_shift)

# surprise surprise, the tory vouches for his tory pals 66% of the time, and lab/lib 16%.

# is this because the previous speakers were often labour/lib?
# seq_tib r got fucked becuase I remvoed "noble lords-"
sequence_tib %<>% mutate(r = row_number())
r_interruptions <- sequence_tib %>% filter(name == "Taylor") %>% pull(r)
rs <- r_interruptions %>% as.list()  %>% map(function(r) c((r-1):(r-1))) %>% unlist() %>% unique()
sequence_tib %>% filter(r %in% rs) %>% mutate(interruption = ifelse(r %in% r_interruptions, T, F)) %>% arrange(r) %>% View()
rs2 <-  r_interruptions %>% as.list()  %>% map(function(r) c((r-1):(r+1))) %>% unlist() %>% unique()
sequence_tib %>% filter(r %in% rs2) %>% mutate(interruption = ifelse(r %in% r_interruptions, T, F)) %>% arrange(r) %>% View()

rsm1 <-  r_interruptions %>% as.list()  %>% map(function(r) c((r-1))) %>% unlist() %>% unique()
rsp1 <- r_interruptions %>% as.list()  %>% map(function(r) c((r+1))) %>% unlist() %>% unique()
sequence_tib %>% filter(r %in% rsm1) %>% mutate(interruption = ifelse(r %in% r_interruptions, T, F)) %>% arrange(r) %>% 
  filter(name != "Taylor") %>% pull(party)
sequence_tib %>% filter(r %in% rsp1) %>% mutate(interruption = ifelse(r %in% r_interruptions, T, F)) %>% arrange(r) %>% 
  filter(name != "Taylor") %>% pull(party)

# some weird interactions where it looks like taylor was interrupted, so the guy
# following him is himself (of course, another tory)

# however, even if we remove these instances, he still passes on to tories quite often
# i.e. 6/10 times

# yeah lets be real he's biased