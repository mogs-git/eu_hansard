# network package stuff

# paired seq tib is an edge list
library(network)
nodes <- appearances_tib %>% distinct(name) %>% select(name) 
routes_network <- network(paired_seq_tib, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
plot(routes_network, vertex.cex = 2, mode = "circle")

# try again but filter out speakers who made only one interaction
multi_pst <- paired_seq_tib %>% filter(weights > 1)
nodes_pst <- tibble(name = unique(c(multi_pst$px, multi_pst$py)))
routes_network <- network(multi_pst, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
plot(routes_network, vertex.cex = 2, mode = "circle")

# Same in igraph

detach(package:network)
rm(routes_network)
library(igraph)

routes_igraph <- graph_from_data_frame(d = multi_pst, vertices = nodes, directed = TRUE)
routes_igraph
plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2, size = 30)


# igraph ----

library(igraph)

# test
g1 <- graph( edges=c(1,2, 2,3, 3,1, 1,4), n=4, directed=F ) 
plot(g1)


# get tidy pairs of lords

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

g2 <- graph.ring( edges=edgy_lords, n=nodes, directed=F ) 
plot(g2, edge.arrow.size=.5, vertex.color="gold", vertex.size=5, 
     
     vertex.frame.color="gray", vertex.label.color="black", 
     
     vertex.label.cex=0.5, vertex.label.dist=1, edge.curved=0.2)


# too many names being plotted, 
V(g2)

# make a boolean for the vertices attribute, saying who should be plotted

to_label <- tibble(edgy_lords) %>% group_by(edgy_lords) %>% count() %>% mutate(to_label = n > 2) 

# mark falses as NAs (necessary to delete from plot)
a <- tibble(edgy_lords) %>% left_join(to_label) %>% mutate(to_label = ifelse(to_label == FALSE, NA, edgy_lords))

# remove duplicated names (but keep in same order of names in vertices attribute)
a <- a[!duplicated(a), ]

to_label <- a$to_label

vertex_attr(g2)
g2 <- set_vertex_attr(graph = g2, name = "to_label", value = to_label)
# g2 <- remove.vertex.attribute(g2, "to_label")

plot(g2, edge.arrow.size=.5, vertex.color="gold", vertex.size=5, 
     
     vertex.frame.color="gray", vertex.label = V(g2)$to_label, vertex.label.color=c("black", "white")[1 + (V(g2)$to_label == TRUE)], 
     
     vertex.label.cex=0.7, vertex.label.dist=1.2, edge.curved=0.2, margin = -0.25)


# weight lines based on n interactions
edge_attr(g2)
E(g2)
g2 <- set_edge_attr(graph = g2, name = "weights", value = paired_seq_tib$weights)
plot(g2, edge.arrow.size=.5, vertex.color=V(g2)$party, vertex.size=5, 
     
     vertex.frame.color="gray", vertex.label = V(g2)$to_label, vertex.label.color=c("black", "white")[1 + (V(g2)$to_label == TRUE)], 
     
     vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.2, margin = -0.25, edge.width = E(g2)$weights)

# party join
main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)", 'other')
pj <- appearances_tib_f %>% mutate(party = ifelse(party %!in% main_parties, "other", party)) %>% select(name, party, gender)
colours <- c("blue", "red", "olivedrab4", "yellow", 'grey')
vertex_colours <- pj[!duplicated(pj),] %>% left_join(tibble(party = main_parties, colour = colours))
vertex_colours <- tibble(name = edgy_lords) %>% left_join(vertex_colours) 
vertex_colours <- vertex_colours[!duplicated(vertex_colours),]
vertex_colours
g2 <- set_vertex_attr(graph = g2, name = "party", value = vertex_colours$colour)


# Set names to surnames
# add edge sizes to graph based on number of interactions at that edge
# add colours to nodes based on party/gender

g <- make_ring(10) %>%
  set_edge_attr("weight", value = 1:10) %>%
  set_edge_attr("color", value = "red")
g
plot(g, edge.width = E(g)$weight)
