# load ----
pacman::p_load(tidyverse, stringr, forcats, purrr, magrittr, tidytext, reshape2, wordcloud)
full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances.RDAT")
`%!in%` <- negate(`%in%`)

tidy_dtm <- appearances_tib %>% 
  mutate(r = row_number()) %>% 
  unnest_tokens(word, speeches) 

eu <- read_file("data/European Union (Notification of Withdrawal) Bill 2017-03-01 (1).txt")
source('scripts\\main_src.R')

# extract ----

names <- extract_names(eu)
appearances_tib <- get_parties(names)

# Manually fix Lords missing parties ----
appearances_tib %>% filter(is.na(party)) %>% unique()

# The Archbishop and Baroness are not assigned a party. 
str_subset(names, "Headley|Elie|York|Scone") %>% unique()

for (i in seq_along(appearances_tib$name)) {
  if (appearances_tib$name[i] == "Lord Bridges of Headley"){
    appearances_tib$party[i] = "(Con)"
  } else if (appearances_tib$name[i] == "Lord Keen of Elie") {
    appearances_tib$party[i] = "(Con)"
  } else if (str_detect(appearances_tib$name[i], "Advocate-General")) {
    appearances_tib$name[i] = "Lord Bridges of Headley"
  } else if (str_detect(appearances_tib$name[i], "Parliamentary Under-Secretary")) {
    appearances_tib$name[i] = "Lord Keen of Elie"
  }
}

# Add more variables ----

appearances_tib %<>% add_gender()

appearances_tib %<>% bind_speeches()

speech_tib <- reduce_speeches(appearances_tib)

saveRDS(appearances_tib, "data//appearances.RDAT")
saveRDS(speech_tib, "data//full_speeches.RDAT")

# The basics: ----

# team composition ----

# PLOT: Number of members of each party coloured by gender.
full_speeches %>% group_by(party, gender) %>% summarise(n = n()) %>%
  ggplot(aes(party, n)) +
  geom_col(aes(fill = gender)) +
  ylab("# members") +
  scale_y_continuous(breaks = seq(0,30,5))


# word counts by gender ----

# Count number of words in each speech (SUMMARY)
(words_per_speech <- appearances_tib %>% mutate(r = row_number()) %>% unnest_tokens(word, speeches) %>% 
  group_by(r, name) %>% summarise(word_count = n()))

# Count total number of words said by each lord
(words_per_lord <- words_per_speech %>% group_by(name) %>% summarise(word_count = sum(word_count)) %>% filter(!is.na(name)))

# Look for pattern in order of speeches
main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
 
seq_plot <- words_per_speech %>% left_join(distinct(appearances_tib, name, party)) %>% mutate(party = ifelse(party %!in% main_parties, "other", party)) %>% ggplot(aes(r, word_count)) + geom_col(aes(fill = party), width = 1) + scale_fill_brewer(palette = "Set1")
seq_plot #+ coord_cartesian(xlim = c(200,250))
appearances_tib %>% mutate(id = row_number(), party = ifelse(party %!in% main_parties, "other", party)) %>% ggplot(aes(id, 1)) + geom_col(aes(fill = party)) + scale_fill_brewer(palette = "Spectral")

hist(words_per_speech$word_count, breaks = 50)
spchs <- words_per_speech %>% filter(word_count > 200, word_count < 250) 
appearances_tib %>% mutate(r = row_number()) %>% semi_join(spchs) %>% View()
full_speeches %<>% mutate(party = ifelse(party %in% main_parties, party, 'other'))
words_per_speech %>% 
  left_join(select(full_speeches, name, party)) %>% ungroup() %>% 
  mutate(next_party = lead(party), next_count = lead(word_count)) %>%
  filter(party != next_party) %>% mutate(s = next_count < word_count + 50) %>% group_by(party, next_party) %>%
  summarise(s = sum(s)) %>% arrange(desc(s))

# take top 10% of talks, who follows who?
words_per_speech %>% 
  left_join(select(full_speeches, name, party)) %>% ungroup() %>% 
  mutate(next_party = lead(party), next_count = lead(word_count)) %>% top_n(25, next_count) %>%
  group_by(party, next_party) %>%
  count() %>% arrange(desc(n))

# running mean of speech lengths
wcs <- words_per_speech$word_count
running_mean <- NULL
for (i in seq_along(wcs)) {
  running_mean[i] <- mean(wcs[1:i])
}
rm_tib <- tibble(id = 1:247, running_mean)
ggplot(rm_tib, aes(id, running_mean)) + geom_point() + geom_line()

# gender distribution in sorted word counts
words_per_lord %<>% add_gender()
words_per_lord %<>% mutate(name =  as.factor(name))
words_per_lord %>% ggplot(aes(x = fct_reorder(name, word_count), y = word_count)) + geom_col(aes(fill = gender)) 

# ladies look evenly distributed among speech lengths

# how often did each party speak?

speeches_by_party <- appearances_tib %>% group_by(party) %>% summarise(n_speeches = n())

# how often would you expect each to speak based on number of lords present?

exp_speeches <- appearances_tib %>% distinct(name, party) %>% group_by(party) %>% summarise(n = n()) %>%
  mutate(total_speeches = 247, nprob = n/sum(n), expected_nspeeches = round(total_speeches*nprob)) 

speeches_by_party %>% left_join(exp_speeches) %>% select(party,n_speeches,expected_nspeeches) %>% 
  gather(speech_type, count, n_speeches, expected_nspeeches) %>%
  ggplot(aes(party, count)) + geom_col(aes(fill = speech_type), position = "dodge")

# Word cloud comparison of speaker pair

head_vs_hayt <- full_speeches %>% mutate(chars_used = str_length(all_speeches)) %>% arrange(desc(chars_used)) %>% top_n(2)
full_speeches %>% mutate(words_used = sum(str_detect(all_speeches, "[^ ]+"), na.rm = TRUE)) %>% select(words_used)

head_vs_hayt %>% 
  unnest_tokens(word, all_speeches) %>%
  anti_join(stop_words) %>% 
  group_by(name) %>%
  count(word) %>% arrange(desc(n)) %>% top_n(9) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~name)

hh_tidy <- head_vs_hayt %>% 
  unnest_tokens(word, all_speeches) %>%
  anti_join(stop_words) %>% 
  group_by(name) %>%
  count(word)

hh_tidy %>% acast(word ~ name, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                 max.words = 100)

# What about the order of speeches ? ----

# Get lenght of speeches
appearances_tib_f <- appearances_tib %>% filter(!str_detect(speeches, "My Lords[^, ]"))
# filter out cases of less than 50 characters

# extract sequence of parties
party_seq <- appearances_tib$party
party_seq[which(is.na(party_seq))] <- "other"
get_seq <- function(vec) {
  grp_cnt <- NULL
  count = 0
  i = 1
  while (i < length(vec)) {
    count  = 0
    j = i
    while (j < length(vec)) {
      if (is.na(vec[i]) | is.na(vec[j])) {
        break
      }
      if (vec[i] == vec[j]) {
        count = count + 1
      } else  if (count > 1) {
        i = i + count - 1
        break
      } else {
        break
      }
      j = j + 1
    }
    grp_cnt <- c(grp_cnt, count)
    i = i + 1
  }
  grp_cnt
}
party_seq_values <- get_seq(party_seq)
party_seq_index <- cumsum(party_seq_values)
party_seq_tib <- tibble(party = party_seq[party_seq_index], run = party_seq_values)
ggplot(party_seq_tib) +
  geom_col(aes(x = party, y = run))

party_seq_tib %<>% mutate(index = row_number())

ggplot(party_seq_tib) +
  geom_col(aes(index, run, fill = party), width = 1)

ggplot(party_seq_tib) +
  geom_histogram(aes(run, fill = party))

# l1 <- as.list(party_seq_tib$party)
# l2 <- as.list(party_seq_tib$run)
# x <- map2(l1, l2, rep) %>% unlist()
# y <- map(l2, ~seq(1, .x)) %>% unlist()
# tibble(x, y) %>% mutate(r = row_number()) %>%
#   ggplot(aes(r, y)) +
#   geom_col(aes(fill = x), width = 1) 
  
# Run is buggy, conservatives sequence always 1 then 2 (why gaps also?)
# I am stupid, this is exactly what I wanted- why the gaps though?

# What about the pairs of speakers ? ----

# make tibble of paired speaker names, then mutate new columsn where names
# are sorted in alphabetical order in each pair. Then reduce + count. 

sequence_tib <- appearances_tib_f %>% select(-speeches)
sequence_tib %<>% mutate(name_shift = lead(name), party_shift = lead(party))
sequence_tib %<>% mutate(px = pmin(name, name_shift), py = pmax(name, name_shift))

paired_seq_tib <-  sequence_tib %>% 
  group_by(px, py) %>%
  summarise(weights = n()) %>% filter(!is.na(px))

total_weights <- paired_seq_tib %>% gather(temp, speaker, -weights) %>% select(-temp) %>% 
  group_by(speaker) %>% summarise(total_weight = sum(weights)) %>% arrange(desc(total_weight))

num_appearances <- appearances_tib_f %>% select(name) %>% group_by(name) %>% count() %>% arrange(desc(n))

# Prior to network plotting and clustering, get raw numbers

paired_seq_tib %>% arrange(desc(weights))
pjoin <-surname_join %>% left_join(select(full_speeches, name, party)) %>% select(surname, party)
main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
pjoin %<>% mutate(party = ifelse(party %in% main_parties, party, 'Other'))
paired_party_tib <- paired_seq_tib %>% ungroup() %>% left_join(pjoin, by = c('px' = 'surname')) %>% left_join(pjoin, by = c('py' = 'surname')) %>%
  mutate(px = pmin(party.x, party.y), py = pmax(party.x, party.y)) 

# px and py are sorted orders, party.x and party.y are unsorted (order as appears)
paired_party_tib %>% group_by(px, py) %>% summarise(total_interactions = sum(weights)) %>% arrange(desc(total_interactions)) %>% View()

paired_party_tib %>% group_by(party.x, party.y) %>% summarise(total_interactions_ordered = sum(weights)) %>% arrange(desc(total_interactions_ordered)) %>% View()


# NETWORKS ----

#https://www.data-imaginist.com/2017/ggraph-introduction-edges/
# GO HERE, TRY SMALL TEST CASE
simple <- make_graph('bull')
# Random names - I swear
V(simple)$name <- c('Thomas', 'Bob', 'Hadley', 'Winston', 'Baptiste')
E(simple)$type <- sample(c('friend', 'foe'), 5, TRUE)

ggraph(simple, layout = 'graphopt') + 
  geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(size = 5)

library(tidygraph)
library(ggraph)
library(igraph)

# instead of names, try numbers
appearance_id <- appearances_tib_f %>% distinct(name) %>% mutate(r = row_number())
edge_list1 <- paired_seq_tib %>% rename(from = px, to = py)#%>% left_join(appearance_id, by = c("px" = "name")) %>% left_join(appearance_id, by = c("py" = "name")) %>% rename(from = r.x, to = r.y) %>% select(-px, -py)
edge_list2 <- edge_list1 %>% mutate(from2 = from) %>% mutate(from = to, to = from2) %>% select(-from2)
edge_list <- bind_rows(edge_list1, edge_list2) 

nodes <-  appearance_id %>% select(name)
routes_tidy <- tbl_graph(nodes = nodes, edges = edge_list, directed = FALSE)
routes_igraph_tidy <- as_tbl_graph(routes_tidy)

routes_igraph_tidy
routes_tidy
ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

# speakers who spoke once
few_int_lords <- total_weights %>% filter(total_weight == 2) %>% pull(speaker)

routes_tidy %>% mutate(name = ifelse(name %!in% few_int_lords, name, " ")) %>%
ggraph(layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weights), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "Interactions") +
  theme_graph() +
  geom_node_text(aes(label = name), colour = 'red', vjust = 0.8)

routes_tidy <- tbl_graph(nodes = nodes, edges = paired_seq_tib, directed = FALSE)
routes_igraph_tidy <- as_tbl_graph(routes_tidy)
ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()



# What can we say about a single speaker? ----

headley_all <- full_speeches %>% filter(name == "Lord Bridges of Headley")

headley <- appearances_tib %>% filter(name == "Lord Bridges of Headley")

hwords <- headley_all$all_speeches 

str_extract(hwords, "\\b.+\\b")
str_extract_all(hwords, "[ ][^ ]*[ ]") %>% head(100)

# most used words
headley_words <- unnest_tokens(headley_all, word, all_speeches) %>% 
  select(-c(name,party, gender))

headley_words %>% group_by(word) %>% summarise(n = n()) %>% arrange(desc(n)) %>% View()

headley_words %>% anti_join(stop_words) %>% group_by(word) %>% summarise(n = n()) %>% arrange(desc(n)) %>% View()

# Most used phrases

headley_bigrams <- unnest_tokens(headley_all, bigram, all_speeches, token = "ngrams", n = 2) %>% 
  select(-c(name,party, gender))

headley_bigrams %>% group_by(bigram) %>% summarise(n = n()) %>% arrange(desc(n)) %>% View()

# get meaningful bigrams (those not where both words are stop words)
stop_words %<>% filter(!str_detect(word, "^.$"))
s_words <- str_c(stop_words$word, collapse = "|")

headley_bigrams %>% group_by(bigram) %>% mutate(sw = str_detect(bigram, str_c("(", s_words, ")", " ", "(", s_words, ")(?!.)"))) %>% 
  filter(!sw) %>% group_by(bigram) %>% summarise(n = n()) %>% arrange(desc(n)) %>% View()

# headley context search, automatically reduced to lower case by tidy text. 

# hwords %>% str_extract_all("[ ][:alnum:]*[ |\\.]")
hwords_collapsed <- str_c(headley_words$word, collapse = " ")
hwords_collapsed %>% str_match_all(".eu [:alnum:]* [:alnum:]*")

regex_patterns <- list()
rgx_nms <- c("eu", "uk", "parliament", "government")
regex_patterns <- str_c(c(".eu", "uk", ".parliament", ".government"),
                        " [:alnum:]* [:alnum:]*")

regex_patterns %<>% set_names(rgx_nms)
regex_patterns

hwords_collapsed %>% str_match_all(regex_patterns[[4]])

# Apply to all lords ----

words_total <- unnest_tokens(speech_tib, word, all_speeches) 
words_total

# sentiment analysis

afinn <- get_sentiments("afinn")
afinn

words_total %>% 
  inner_join(afinn) %>%
  group_by(name) %>% 
  filter(!str_detect(word, "noble|Noble|lord|Lords|Lord")) %>%
  summarise(s = sum(score), n = length(word)) %>%
  arrange(s) %>%
  ggplot(aes(name, s/sqrt(n))) +
  geom_point(aes(size = n)) +
  coord_flip()

# topic modelling

library(tm)
library(topicmodels)

dtm <- appearances_tib %>% 
  mutate(r = row_number()) %>% 
  unnest_tokens(word, speeches) %>% 
  group_by(name, r, word) %>% 
  summarise(count = length(word)) %>%
  arrange(r)

dtm %<>% ungroup() %>% anti_join(stop_words)

dtm %<>% filter(word %!in% my_stop_words)

dtm %<>% group_by(r) %>% mutate(n_words = sum(count)) 
docLength <- dtm %>% distinct(r, n_words)
hist(docLength$n_words, breaks = 100)
dtm %<>% filter(n_words < 50) # do this before removing stop words
full_speeches$all_speeches %>% str_extract_all("Amendment.........")

# Number of amendments 36,21,43,17,20,18,19,34,16,33,44,39,11,29,9B,40,25,31,38,35,12,13,10
n_amendments <- c(36,21,43,17,20,18,19,34,16,33,44,39,11,29,9,40,25,31,38,35,12,13,10)

dtm_obj <- cast_dtm(dtm, r, word, count)

dtm_obj

hansard_lda <- LDA(dtm_obj, k = 5, control = list(seed = 1234))

hansard_lda

h_topics <- tidy(hansard_lda, matrix = "beta")

hansard_top_terms <- h_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

hansard_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

beta_spread <- h_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > 0.001 | topic2 > 0.001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

# Topic modelling using LDA feel like too much of a black box.

# New approach:

tidy_dtm <- appearances_tib %>% 
  mutate(r = row_number()) %>% 
  unnest_tokens(word, speeches) %>% 
  group_by(name, r, word) %>% 
  summarise(count = length(word)) %>%
  arrange(r)

tidy_dtm %<>% group_by(r) %>% mutate(n_words = sum(count))

tidy_dtm %>% distinct(n_words)

appearances_tib$speeches[[4]] # 200 words seems like the lower limit for an informative document

tidy_dtm %<>% filter(n_words > 200)

tidy_dtm %<>% anti_join(stop_words)

my_stop_words <- c("lord", "noble", "friend", "lords", "house", "minister", "1", "lordships")

tidy_dtm %<>% filter(word %!in% my_stop_words)

tidy_dtm %<>% filter(count > 1)

tidy_dtm %<>% group_by(r, word) %>% mutate(doc_word_frequency = count/n_words, norm_dwf = (doc_word_frequency - min(doc_word_frequency)) - (max(doc_word_frequency) - min(doc_word_frequency))) %>%
  ungroup() %>% mutate(total_words = sum(count)) %>% group_by(word) %>% mutate(corpus_word_freq = sum(count)/total_words) 

tidy_dtm %>% distinct(word, corpus_word_freq) %>% arrange(desc(corpus_word_freq)) %>% View()

top_dtm <- tidy_dtm %>% arrange(r, desc(doc_word_frequency)) %>% group_by(r) %>% top_n(10, doc_word_frequency) %>% mutate(wordsInSpeech = length(word))
top_dtm %<>% group_by(r, doc_word_frequency) %>% mutate(nrepeats = n(), tooMany = ifelse(nrepeats > wordsInSpeech/3 & count < 3, TRUE, FALSE)) %>% filter(!tooMany)

top_words <- top_dtm %>% split(.$r) %>% map("word")

not_shared <- list()
for (i in seq_along(top_words)) {
  if(i != 1) {
    not_shared[[i]] <-  top_words[[i]][top_words[[i]] %!in% top_words[[i-1]]]
  }
  if(i == 1){
    not_shared[[i]] <- top_words[[i]]
  }
}

shared <- list()
for (i in seq_along(top_words)) {
  if(i != 1) {
    shared[[i]] <-  top_words[[i]][top_words[[i]] %in% top_words[[i-1]]]
  }
  if(i == 1){
    shared[[i]] <- top_words[[i]]
  }
}


not_shared
shared


tidy_dtm$doc_word_frequency %>% sort() %>% as.tibble() %>% mutate(r = row_number(), l = length(r), p = r/l) %>%
  ggplot(aes(value, p)) + geom_line()


# counting even more things ----

# Number of questions

full_speeches %>% group_by(name) %>% mutate(numQs = str_count(all_speeches, "\\?")) %>% View()

s <- full_speeches %>% filter(name == "Lord Oates") %>% pull(all_speeches)
qwords <- "(\\Wdo|\\Wwhat|\\Wwhere|\\Wwho|\\Wwhy|\\Wwhen|\\Whow|\\Wif|\\Whas|\\Wwill|\\?)"
str_extract_all(s, "\\w+\\?")
str_extract_all(s, "\\?")
initial_matches <- str_extract_all(s, qwords)[[1]]
initial_match_indexes_start <- which(initial_matches == "?") - 1
initial_match_indexes_end <- which(initial_matches == "?")
qword_positions <- str_locate_all(s, qwords)[[1]]
str_sub(s, qword_positions[initial_match_indexes_start], qword_positions[initial_match_indexes_end])

# Pull questions
pull_questions <- function(string) {
  # pull the questions asked in a string, and the question word used.
  qwords <- "(\\Wdo|\\Wwhat|\\Wwhere|\\Wwho|\\Wwhy|\\Wwhen|\\Whow|\\Wif|\\Whas|\\Wwill|\\Wdoes|\\?)"
  initial_matches <- str_extract_all(string, qwords)[[1]]
  initial_match_indexes_start <- which(initial_matches == "?") - 1
  initial_match_indexes_end <- which(initial_matches == "?")
  qword_positions <- str_locate_all(string, qwords)[[1]]
  question_sentence <- str_sub(string, qword_positions[initial_match_indexes_start], qword_positions[initial_match_indexes_end])
  question_word <- initial_matches[initial_match_indexes_start]
  
  sentence_matches <- str_extract_all(string, "\\.|\\:|\\?")[[1]]
  sentence_matches_start <- which(sentence_matches == "?") - 1
  sentence_matches_end <- which(sentence_matches == "?")
  qsentence_positions <- str_locate_all(string, "\\.|\\:|\\?")[[1]]
  question_sentence_full <- str_sub(string, qsentence_positions[sentence_matches_start], qsentence_positions[sentence_matches_end])
  
  return(tibble(question_word, question_sentence, question_sentence_full))
}

pull_questions(s) %>% View()
# Number of unique words used

total_words <- unique(tidy_dtm$word)
tidy_dtm %>% group_by(word) %>% mutate(total_count = n()) %>%
  group_by(name) %>% summarise(total_words = sum(count), num_unique = sum(total_count == count), prop_unique = num_unique/total_words) %>%
  arrange(desc(prop_unique))

# Number of times other lords are mentioned

surname <- full_speeches$name %>% str_match("^(?:\\w+\\s)(\\w+)") 
surname <- surname[,2] %>% str_to_lower()
for (i in seq_along(surname)) {
  surname[i] <- str_c("\\s", surname[i], "\\W")
}

surname <- surname[which(surname != '\\sO\\W' & surname != '\\sDe\\W')]
pronouns <- c('his lordship', 'the baroness', 'the lord', 'the minister', 'her ladyship', 'the noble lords?(?!(.{0,4}lord))', 'reverend primate')
named_references <- c(surname, pronouns) 
named_references_reg <- str_c("(", str_c(named_references, collapse = "|"), ")")

# Problems: Some names match other words e.g. 'true', 'hunt'. double barrelled names. 
# instances where someone refers to 'the noble lord, lord blencathra' will be counted twice
# could solve this using negative lookahead on 'the noble'
appearances_tib %<>% mutate(speeches = str_to_lower(speeches)) %>% filter(!is.na(speeches))

appearances_tib %>% group_by(name) %>% mutate(n_references = str_count(speeches, named_references_reg)) %>% View()

s <- appearances_tib %>% filter(name == 'Lord Winston') %>% pull(speeches) 
s <- appearances_tib$speeches[[1]] 
s <- "The noble Lord, Lord Howard, and his noble steed carrying him, are followed by Lord Chubbington and his noble donkey, Assworth. There was supposedly a third lord, but the noble Lord being too drunk, did not arrive for the adventure."
str_view_all(s, named_references_reg)
str_extract_all(s, named_references_reg)
str_view_all(s, "noble Lords?(?!(.{0,2}Lord))")
# want to find number of qs per x many words

# word associations

assoc_pattern <- function(word) {str_c("\\.[^.]+\\W", word, "(\\.|\\W[^.]+?(?=\\.))")}
str_extract_all(s, assoc_pattern("leave"))

eu_docs <- map(appearances_tib$speeches, str_extract_all, assoc_pattern("eu"))
head(eu_docs)
eu_docs %<>% flatten() %>% flatten() 

tibble(doc = unlist(eu_docs)) %>% mutate(r = row_number()) %>% group_by(r) %>% unnest_tokens(word, doc) %>% 
  anti_join(stop_words) %>% group_by(word) %>% summarise(count = n()) %>% arrange(desc(count)) %>% View()



# Attempt at topic modelling ----

tidy_dtm %>% 
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]")) %>%
  group_by(word, r) %>% 
  mutate(speech_count = n()) %>%
  group_by(word) %>%
  mutate(total_count = n()) %>%
  group_by(r) %>%
  mutate(words_in_speech = n()) %>%
  ungroup() %>%
  mutate(words_in_hansard = n()) %>% arrange(desc(speech_count),r) %>% distinct() %>% View()

topic_frame <- tidy_dtm %>% 
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]")) %>%
  group_by(word, r) %>% 
  mutate(speech_count = n()) %>%
  group_by(word) %>%
  mutate(total_count = n()) %>%
  group_by(r) %>%
  mutate(words_in_speech = n()) %>%
  ungroup() %>%
  mutate(words_in_hansard = n()) %>% arrange(desc(speech_count),r) %>% distinct() %>% 
  mutate(speech_prop = speech_count/words_in_speech) %>%
  mutate(total_prop = total_count/words_in_hansard) %>%
  filter(total_prop<speech_prop) %>% arrange(r) 

topic_words <- topic_frame %>% filter(speech_count > 2) %>% split(.$r) %>% map("word")
txt <- topic_words[[1]]
# Idea here is to only pull the nouns from the text
# "Part of speech tagging"

library(NLP)
library(openNLP)
library(openNLPmodels.en)

# Some text.
s <-paste(c("John likes the girl.\n","The girl also likes John."),collapse = "")
s <-as.String(s)
## Need sentence and word token annotations.
sent_token_annotator <-Maxent_Sent_Token_Annotator()
word_token_annotator <-Maxent_Word_Token_Annotator()
a2 <-annotate(s, list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <-Maxent_POS_Tag_Annotator()
pos_tag_annotator
#>An annotator inheriting from classes
#>  Simple_POS_Tag_Annotator Annotator
#>with description
#>  Computes POS tag annotations using the Apache OpenNLP Maxent Part of
#>  Speech tagger employing the default model for language 'en'
a3 <-annotate(s, pos_tag_annotator, a2)
a3
s

## Some text.
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)
## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(s, pos_tag_annotator, a2)
a3
## Variant with POS tag probabilities as (additional) features.
head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))

## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)
## Extract token/POS pairs (all of them): easy.
sprintf("%s/%s", s[a3w], tags)
## Extract pairs of word tokens and POS tags for second sentence:
a3ws2 <- annotations_in_spans(subset(a3, type == "word"),
                              subset(a3, type == "sentence")[2L])[[1L]]
sprintf("%s/%s", s[a3ws2], sapply(a3ws2$features, `[[`, "POS"))


# Try and apply this stuff to my stuff

library(NLP)
library(openNLP)
library(openNLPmodels.en)

appearances_tib %>% rowwise() %>%  summarise(s = str_length(speeches))
otxt <- appearances_tib$speeches[[6]]
txt <- appearances_tib$speeches[[6]]
txt <- as.String(txt)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(txt, list(sent_token_annotator, word_token_annotator))

pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(txt, pos_tag_annotator, a2)
a3
## Variant with POS tag probabilities as (additional) features.
head(annotate(txt, Maxent_POS_Tag_Annotator(probs = TRUE), a2))

## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)
sprintf("%s/%s", txt[a3w], tags)

# Voi fucking LA

pos_tag_abbrs <- readRDS("data/pos_tag_abbrs.RDAT")

pos_tag_abbrs %>% head(25)
tags_f <- tags[str_detect(txt[a3w], "[A-Za-z]")]
txt[a3w]
tibble(tags) 
which(str_detect(tags, "NN"))
txt_tib <- tibble(otxt) 
txt_tib %>% unnest_tokens(word, otxt) %>% mutate(id = row_number()) %>% add_column(tags_f) %>%
  filter(str_detect(tags_f, "NN"))
annotate(txt, Maxent_POS_Tag_Annotator(probs = TRUE), a2)
txt[a3w]



# Function to annotate a specific text

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

anno <- function(otxt, pat = "NN") {
  txt <- as.String(otxt)
  a2 <- annotate(txt, list(sent_token_annotator, word_token_annotator))
  a3 <- annotate(txt, pos_tag_annotator, a2)
  #a3probs <- annotate(txt, Maxent_POS_Tag_Annotator(probs = TRUE), a2)
  #a3pw <- subset(a3probs, type == "word")
  #probs <- sapply(a3pw$features, `[[`, "POS_prob")
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  tags_f <- tags[str_detect(txt[a3w], "[A-Za-z]")]
  txt_tib <- tibble(word = txt[a3w], tags)
  txt_tib %>%
    filter(str_detect(tags, pat)) %>% left_join(pos_tag_abbrs, by = c("tags" = "abbreviation"))
} 

o <- anno(otxt, pat = "[A-Za-z]")

otxt <- appearances_tib$speeches[[1]]
otxt = "expatriate"

# also use bigrams (e.g. adj + noun) to model topics
tidy_bigrams <- appearances_tib %>% 
  mutate(r = row_number()) %>% 
  unnest_tokens(bigram, speeches, token = "ngrams", n = 2) 

swords <- stop_words %>% filter(!str_detect(word, "^.$"))
s_words <- str_c(swords$word, collapse = "|")

tidy_bigrams %>% filter(r == 1) %>% group_by(bigram) %>% mutate(sw = str_detect(bigram, str_c("(", s_words, ")", " ", "(", s_words, ")(?!.)"))) %>% 
  filter(!sw) %>% group_by(bigram) %>% summarise(n = n()) %>% arrange(desc(n)) %>% View()

# POS annotated tibbles 
annos_nested <- appearances_tib %>% 
  mutate(r = row_number()) %>%
  group_by(r) %>%
  nest() %>%
  mutate(s = map(data, "speeches")) %>%
  mutate(annotations = map(s, anno, ".*"))

annos_nested$annotations[[1]]  %>% distinct(word, tags)
annos_nested$annotations[[1]] %>% filter(!str_detect(word, "\\W")) %>%
  mutate(nword = lead(word), ntag = lead(tags), ndesc = lead(description)) %>%
  unite(bigram, word, nword, sep = " ") %>%
  unite(bitag, tags, ntag, sep = " ") %>% 
  filter(str_detect(bitag, "JJ[S|R]? NN[S|P|PS]?")|str_detect(bitag, "NN[S|P|PS]? NN[S|P|PS]?")) %>% View()



tidy_dtm_tf <- tidy_dtm %>% group_by(r) %>% count(word)
tfidf <- tidy_dtm_tf %>% bind_tf_idf(word, r, n)
tfidf %>% arrange(desc(tf_idf)) %>%  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  filter(r %in% 1:6) %>%
  group_by(r) %>% top_n(6, tf_idf) %>% ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = r)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~r, ncol = 2, scales = "free") +
  coord_flip()

tfidf %>% arrange(desc(tf_idf)) %>%  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  filter(r %in% 1:6) %>% group_by(r) %>% summarise(s = sum(n))


party_tib <- appearances_tib %>% mutate(r = row_number()) %>% distinct(r, party)
corplot <- tidy_dtm_tf %>% mutate(tot = sum(n)) %>% filter(tot > 500) %>%
  anti_join(stop_words) %>% mutate(p = n/tot) %>% subset(n >= 3) %>%
  select(-n, -tot) %>% left_join(party_tib) %>% unite(r, party, r, sep = "_") %>%
  spread(r, p) 

corplot[is.na(corplot)] <- 0 

mycol <- colorRampPalette(c("darkgrey", "grey", "white", "cadetblue1", "cadetblue"))  
corr <- cor(corplot[,-1], use = "pairwise.complete.obs") %>%  
  corrplot::corrplot(method="color", order="hclust", diag=FALSE, 
           tl.col = "black", tl.srt = 45, tl.cex=0.6,
           col=mycol(100), 
           type="lower",
           title="Correlation Between speech styles", 
           family="Avenir",
           mar=c(0,0,1,0))

corr

library(tidygraph)
library(ggraph)

tfidf
word_dist <- dist(corplot)
attr(word_dist, which = "Labels") <- names(corplot)[-1] 
clustdat <- hclust(word_dist)
clustdat$labels
word_tree <- as_tbl_graph(clustdat)
word_tree

corplot
clustdat
clustgroups <- cutree(clustdat, k = 4)
clustgroups

word_tree %<>% activate(nodes) %>% mutate(party = str_extract(label, ".\\w*."))
word_tree1 <- word_tree %>%
  mutate(party = map_bfs_back_chr(node_is_root(), .f = function(node, path, ...) {
    nodes <- .N()
    if (nodes$leaf[node]) return(nodes$party[node])
    if (anyNA(unlist(path$result))) return(NA_character_)
    path$result[[1]]
  }))

word_tree
word_tree1 %>% activate(nodes) %>% ggraph(layout = 'dendrogram') + 
  geom_edge_diagonal2(aes(colour = node.party)) +
  geom_node_point(aes(filter = leaf, colour = party)) + 
  theme_graph()



corplot_t <- t(corplot[,-1])

# calculate distance
word_dist <- dist(corplot_t, method="euclidean")

# fit clusters
fit <- hclust(word_dist, method="average")

# plot 
plot(fit,main="Cluster Dendrogram of Beer Styles",  
     family="Avenir")
rect.hclust(fit, k=3, border="cadetblue")  
