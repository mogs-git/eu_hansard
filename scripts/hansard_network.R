library(tidygraph)
library(ggraph)
library(igraph)

pacman::p_load(tidyverse, magrittr, tidytext, stringr)

full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances.RDAT")
party_id_tib <- readRDS("data//party_id_tib.RDAT")
`%!in%` <- negate(`%in%`)

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
mycols <- c("dodgerblue3", "forestgreen", "goldenrod1", "firebrick2", "grey30")


# Main centrality plot
tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% topnames | value.y %in% topnames, 1, 0.2)) %>%
  ggraph(layout = 'lgl') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  scale_colour_manual(values = mycols) +
  geom_node_label(aes(filter = top, label = name, alpha = 0, size = 0.2), nudge_x = 0.25, nudge_y = -0.3, label.padding = unit(0.1, 'lines')) +
  theme_graph()

# interactions of one party
tidygraph %>%
  mutate(top_edge = ifelse(value.x %in% tory_lords | value.y %in% tory_lords, 1, 0.2)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = top_edge, width = weights)) + 
  scale_edge_width(range = c(0.5,1.5)) +
  geom_node_point(aes(size = centrality, colour = party)) +
  scale_colour_manual(values = mycols) +
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
    scale_edge_width(range = c(0.5,1.5)) +
    scale_edge_alpha_manual(values = c(0, 1)) +
    scale_edge_colour_manual(values = clrs) +
    geom_node_point(aes(filter = lord_group, size = centrality)) +
    #geom_node_label(aes(filter = lord_group, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
    theme_graph() + theme(legend.position="none")
}
pure_party_edge_plot(tidygraph, tory_lords, c("blue", "blue"))
ggpubr::ggarrange(pure_party_edge_plot(tidygraph, tory_lords, c(mycols[1], mycols[1])),
                  pure_party_edge_plot(tidygraph, labour_lords, c(mycols[4], mycols[4])),
                  pure_party_edge_plot(tidygraph, libdem_lords, c(mycols[3], mycols[3])),
                  pure_party_edge_plot(tidygraph, cb_lords, c(mycols[2], mycols[2])))
                  

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
    #scale_edge_colour_manual(values = clrs) +
    geom_node_point(aes(filter = speaker_cons|t4, colour = t4, shape = lord_group)) +
    #geom_node_label(aes(filter = speaker_cons, label = name, alpha = 0, size = 0.02), nudge_x = 0, nudge_y = 2, label.padding = unit(0.1, 'lines')) +
    theme_graph() + theme(legend.position="none") + ggtitle(speaker)
}

seedcoords <- ss_edge_plot(tidygraph, "Howard", top4, "dh")
coords <- create_layout(tidygraph, "dh") %>% select(x, y)

top4 <- speech_counts %>% head(4) %>% pull(name)
set.seed(1234)
top4_networks <- map(top4, ~ss_edge_plot(tidygraph, ., top4, coords))
ggpubr::ggarrange(top4_networks[[1]], top4_networks[[2]], top4_networks[[3]], top4_networks[[4]],
                  nrow = 2, ncol = 2)

top4_networks[[4]]


# Labour + Tory edges

tidygraph %>%
  mutate(edge_col = ifelse(value.x %in% labour_lords & value.y %in% labour_lords, "labour", 
                              ifelse(value.x %in% tory_lords & value.y %in% tory_lords, "tory",
                                     ifelse((value.x %in% tory_lords & value.y %in% labour_lords) | (value.x %in% labour_lords & value.y %in% tory_lords), "mixed", "NA")))) %>%
  activate(nodes) %>%
  mutate(labourortory = ifelse(name %in% labour_lords | name %in% tory_lords, TRUE, FALSE),
         labour = ifelse(name %in% labour_lords, TRUE, FALSE)) %>%
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(filter = edge_col == "labour"|edge_col == "tory"|edge_col == "mixed", alpha = edge_col, colour = edge_col, width = weights)) +
  scale_edge_width(range = c(0.5,1.5)) +
  scale_edge_alpha_manual(values = c(1, 0.8,1,0)) +
  scale_edge_colour_manual(values = c("red", "purple", "blue", "grey")) +
  geom_node_point(aes(filter = labourortory, size = centrality, colour = labour)) +
  scale_colour_manual(values = c("blue", "red")) +
  geom_node_label(aes(filter = labourortory, label = name, alpha = 0, size = 0.2), nudge_x = 0, nudge_y = 0.4, label.padding = unit(0.1, 'lines')) +
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

# Simpler features that inform our understanding of the network

# Which lords spoke most often?
speech_counts <- appearances_tib_f %>% group_by(name) %>% count() %>% arrange(desc(n)) 
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
https://christophergandrud.github.io/networkD3/#simple
install.packages("networkD3")

subgrps <- cluster_walktrap(tidygraph)
members <- membership(subgrps)

d3_tidygraph <- networkD3::igraph_to_networkD3(tidygraph, group = members)

# Create force directed network plot
networkD3::forceNetwork(Links = d3_tidygraph$links, Nodes = d3_tidygraph$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')


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