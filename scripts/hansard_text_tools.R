# Tools for text analysis
pacman::p_load(tidyverse, magrittr, tidytext, stringr, igraph, ggraph, tidygraph, grid, gridExtra, ggpubr, networkD3)
appearances_tib_f <- readRDS("data//appearances_f.RDAT")
appearances_tib_fp <- appearances_tib_f %>% select(-party) %>% rename(party = party_f) %>% mutate(speech_id = row_number())

# general functions -------------------------------------------------------

list.append <- function(l, val) {
  pos = length(l) 
  l[[pos+1]] <- val
  l
}

test_speech <- appearances_tib_fp$speeches[[1]]

# Phrase search -----------------------------------------------------------

# Search some text blindly for phrases (n-grams where n > 1)

df <- appearances_tib_fp %>% 
  mutate(speech_id = row_number()) %>%
  filter(speech_id == 1)

search_phrases <- function(df) {
  if (!is.data.frame(df)) {
    df <- tibble(speeches = df)
  }
  phrase_books <- list()
  for (i in 2:6) {
    toAppend <- df %>% unnest_tokens(ngram, speeches, token = "ngrams", n = i)
    phrase_books <- list.append(phrase_books, toAppend)
  }
  
  common_phrases <- map(phrase_books, count, ngram) %>% map(~filter(., n > 1))
  
  detect_all_stops <- function(df) {
    df %<>%
      mutate(ngram_split = str_split(ngram, " ")) %>%
      mutate(n_stopwords = map_int(ngram_split, ~sum(. %in% stop_words$word))) %>%
      mutate(ngram_length = map_int(ngram_split, length)) %>%
      mutate(allstops = ngram_length == n_stopwords)
  }
  
  common_phrases %<>% map(detect_all_stops) %>% map(~filter(., !allstops))
  
  rlist <- list()
  for (i in 1:(length(common_phrases) -1)) {
    rlist[[i]] <- expand.grid(common_phrases[[i]]$ngram, common_phrases[[i+1]]$ngram) %>% as.tibble() %>%
      mutate_all(as.character) %>% rowwise() %>%
      mutate(dup = str_detect(Var2,Var1)) %>%
      filter(dup) %>% select(Var1)
  }
  rjoin <- rlist %>% reduce(bind_rows) %>% distinct() %>% rename(ngram = Var1)

  common_phrases %<>%
    reduce(bind_rows) %>%
    select(-n_stopwords, -allstops, -ngram_split)
  
  common_phrases %<>% anti_join(rjoin)
  
  common_phrases
}

# Example uses

search_phrases(df) %>% group_by(ngram_length) %>% top_n(1, n)

appearances_tib_fp %>% 
  mutate(speech_id = row_number()) %>%
  filter(speech_id == 25) %>%
  search_phrases() %>% group_by(ngram_length) %>% top_n(1, n)

appearances_tib_fp %>%
  head(3) %>% 
  select(surname, speeches) %>% 
  mutate(speech_id = row_number()) %>%
  rowwise() %>%
  mutate(phrases = list(search_phrases(speeches)%>% group_by(ngram_length) %>% top_n(1, n))) %>% View()


# Words in context --------------------------------------------------------

wordInContext_rgx <- function(txt, word) {
  # lower case full string vector (txt) and search term (word).
  txt %>% str_extract_all(str_c("(^\\W*|\\w+\\W+)", word, "s?(\\W+\\w+|\\W*$)"))
}

wordInContext_rgx(str_to_lower(df$speeches), "eu")

wordInContext_sentence <- function(txt, word) {
  # lower case full string vector (txt) and search term (word).
  txt %>% str_extract_all(str_c("([^\\.\\?\\!]*\\s)", word, "(\\s|,|:|;)[^\\.\\?\\!]*"))
}

wordInContext_sentence(str_to_lower(df$speeches), "eu")

# str_view_all(". . . . . . . .a a a a  aa . a. aaa a a. aaa", "\\s") REGEX TEST


# POS tagging -------------------------------------------------------------

pacman::p_load(NLP, openNLP, openNLPmodels.en)

# Function to annotate a specific text

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_abbrs <- readRDS("data/pos_tag_abbrs.RDAT")

anno <- function(text, pattern = "NN") {
  # Note: pattern = .* will return tags for all words in text
  
  # Tokenizes the text into sentences and words
  text <- as.String(text)
  a2 <- annotate(text, list(sent_token_annotator, word_token_annotator))
  
  a3 <- annotate(text, pos_tag_annotator, a2)
  
  #a3probs <- annotate(txt, Maxent_POS_Tag_Annotator(probs = TRUE), a2)
  #a3pw <- subset(a3probs, type == "word")
  #probs <- sapply(a3pw$features, `[[`, "POS_prob")
  
  # Take the words from the text, pull the POS features
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  
  #tags_f <- tags[str_detect(text[a3w], "[A-Za-z]")]
  
  # Make a tibble of the words and tags, filter for the desired part-of-speech (pattern)
  text_tib <- tibble(word = text[a3w], tags)
  text_tib %>%
    filter(str_detect(tags, pattern)) %>% left_join(pos_tag_abbrs, by = c("tags" = "abbreviation"))
} 

annos_nested <- appearances_tib_fp %>% 
  mutate(speech_id = row_number()) %>%
  group_by(speech_id) %>%
  nest() %>%
  mutate(s = map(data, "speeches")) %>%
  mutate(annotations = map(s, anno, ".*"))

annos_nested$annotations[[1]]  %>% distinct(word, tags)

annos_nested$annotations[[1]] %>% filter(!str_detect(word, "\\W")) %>%
  mutate(nword = lead(word), ntag = lead(tags), ndesc = lead(description)) %>%
  unite(bigram, word, nword, sep = " ") %>%
  unite(bitag, tags, ntag, sep = " ") %>% 
  filter(str_detect(bitag, "JJ[S|R]? NN[S|P|PS]?")|str_detect(bitag, "NN[S|P|PS]? NN[S|P|PS]?")) %>% View()



# Questions ---------------------------------------------------------------

pull_questions <- function(string) {
  # pull the questions asked in a string, and the question word used.
  qwords <- "(\\Wdo|\\Wwhat|\\Wwhere|\\Wwho|\\Wwhy|\\Wwhen|\\Whow|\\Wif|\\Whas|\\Wwill|\\Wdoes|\\?)"
  # initial_matches <- str_extract_all(string, qwords)[[1]]
  # initial_match_indexes_start <- which(initial_matches == "?") - 1
  # initial_match_indexes_end <- which(initial_matches == "?")
  # qword_positions <- str_locate_all(string, qwords)[[1]]
  # question_sentence <- str_sub(string, qword_positions[initial_match_indexes_start], qword_positions[initial_match_indexes_end])
  # question_word <- initial_matches[initial_match_indexes_start]
  # 
  sentence_matches <- str_extract_all(string, "\\.|\\:|\\?|\\!")[[1]]
  sentence_matches_start <- which(sentence_matches == "?") - 1
  for (i in seq_along(sentence_matches_start)) {
    if (sentence_matches_start[[i]] == 0) {
      sentence_matches_start[[i]] <- sentence_matches_start[[i]] + 1
    }
  }
  sentence_matches_end <- which(sentence_matches == "?")
  qsentence_positions <- str_locate_all(string, "\\.|\\:|\\?")[[1]]
  question_sentence_full <- str_sub(string, qsentence_positions[sentence_matches_start], qsentence_positions[sentence_matches_end])
  all_qwords <- str_extract_all(str_to_lower(question_sentence_full), qwords)
  first_qword <- all_qwords %>% map(~`[[`(., 1))
  return(tibble(first_qword, all_qwords, question_sentence_full))
}

# pull_questions <- function(string) {
#   # pull the questions asked in a string, and the question word used.
#   qwords <- "(\\Wdo|\\Wwhat|\\Wwhere|\\Wwho|\\Wwhy|\\Wwhen|\\Whow|\\Wif|\\Whas|\\Wwill|\\Wdoes|\\?)"
#   # initial_matches <- str_extract_all(string, qwords)[[1]]
#   # initial_match_indexes_start <- which(initial_matches == "?") - 1
#   # initial_match_indexes_end <- which(initial_matches == "?")
#   # qword_positions <- str_locate_all(string, qwords)[[1]]
#   # question_sentence <- str_sub(string, qword_positions[initial_match_indexes_start], qword_positions[initial_match_indexes_end])
#   # question_word <- initial_matches[initial_match_indexes_start]
#   # 
#   sentence_matches <- str_extract_all(string, "\\.|\\:|\\?|\\!")[[1]]
#   sentence_matches_start <- which(sentence_matches == "?") - 1
#   sentence_matches_end <- which(sentence_matches == "?")
#   qsentence_positions <- str_locate_all(string, "\\.|\\:|\\?")[[1]]
#   question_sentence_full <- str_sub(string, qsentence_positions[sentence_matches_start], qsentence_positions[sentence_matches_end])
#   all_qwords <- str_extract_all(str_to_lower(question_sentence_full), qwords)
#   first_qword <- all_qwords %>% map(~`[[`(., 1))
#   return(tibble(first_qword, all_qwords, question_sentence_full))
# }

# Pull questions
pull_questions(test_speech) %>% View()

# Validate
str_count(test_speech, "\\?")



# bigrams -----------------------------------------------------------------

bigrams_from_df <- function(df, text_var, rm_stopwords = F) {
  text_var_q  <- rlang::enexpr(text_var)

  tokenized_df <- df %>% 
    mutate(speech_id = row_number()) %>% 
    unnest_tokens(bigram, !!text_var_q, token = "ngrams", n = 2) 
  
  if (rm_stopwords) {
    tokenized_df %<>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!(word1 %in% stop_words$word & word2 %in% stop_words$word)) %>%
      unite(bigram, word1, word2, sep = " ")
  }
  return(tokenized_df)
}

bigrams <- appearances_tib_fp[1,] %>%
  bigrams_from_df(speeches, rm_stopwords = T)

# Standard analysis

# counts

(bigram_counts <- bigrams %>%
  count(bigram, sort = T))

bigrams_to_graph <- function(bigram_counts) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigram_counts %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(n > 1) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    scale_edge_alpha(0.5,1) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

bigrams_to_graph(bigram_counts)


# correlating words -------------------------------------------------------
library(widyr)

tidy_df <- appearances_tib_fp %>%
  mutate(speech_id = row_number()) %>%
  filter(name == "Lord Bridges of Headley") %>%
  unnest_tokens(word, speeches) %>%
  filter(!word %in% stop_words$word)

# count words co-occuring within sections
word_pairs <- tidy_df %>%
  pairwise_count(word, speech_id, sort = TRUE)

word_pairs %>% filter(item1 == "eu")

# Phi coefficient
word_cors <- tidy_df %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, speech_id, sort = TRUE)

# Words associated most often with other words
word_cors %>%
  filter(item1 %in% c("eu", "government", "citizens", "leave")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

word_cors %>%
  filter(correlation > .75) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  scale_edge_alpha(range = c(0.01,0.25)) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

word_cors$correlation %>% boxplot()

# targeted sentiment analysis ---------------------------------------------

Headley_speeches <- appearances_tib_fp %>% filter(surname == "Bridges")
Hayter_speeches <- appearances_tib_fp %>% filter(surname == "Hayter")

# Get some sentences
sents_hayter <- wordInContext_sentence(str_to_lower(Hayter_speeches$speeches), "eu") %>% unlist()
sents_headley <- wordInContext_sentence(str_to_lower(Headley_speeches$speeches), "eu") %>% unlist()

length(sents_hayter)
length(sents_headley)

# get sentiment scores
AFINN <- get_sentiments("afinn")

confounding_words <- c("lord", "lords")

sents_to_tib <- function(sentence_vector) {
  tibble(sents = sentence_vector, sent_id = 1:length(sentence_vector)) %>%
    unnest_tokens(word, sents)
}

hayter_score <- sents_hayter %>%
  sents_to_tib() %>%
  inner_join(AFINN) %>%
  filter(!word %in% confounding_words) %>%
  group_by(sent_id) %>%
  summarise(sent_score = sum(score), nwords = n()) %>%
  mutate(speaker = "Hayter")

headley_score <- sents_headley %>%
  sents_to_tib() %>%
  inner_join(AFINN) %>%
  filter(!word %in% confounding_words) %>% 
  group_by(sent_id) %>%
  summarise(sent_score = sum(score), nwords = n()) %>%
  mutate(speaker = "Headley")

bind_rows(hayter_score, headley_score) %>%
  ggplot(aes(speaker, sent_score)) + geom_boxplot() + geom_point(alpha = 0.3)

headley_score %>% arrange(desc(sent_score)) %>% head()
hayter_score %>% arrange(desc(sent_score)) %>% head()
sents_headley[18]
sents_hayter[8]


# lexrank: representative sentences ---------------------------------------

library("lexRankr")

speech_sentences <- appearances_tib_fp %>% 
  mutate(speech_id = row_number()) %>%
  unnest_sentences(sentences, speeches)

pt <- speech_sentences %>%
  filter(speech_id == 86) %>%
  pull(sentences)

top_3 = lexRankr::lexRank(pt,
                          #only 1 article; repeat same docid for all of input vector
                          docId = rep(1, length(pt)),
                          #return 3 sentences to mimick /u/autotldr's output
                          n = 3,
                          continuous = TRUE)

#reorder the top 3 sentences to be in order of appearance in article
order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
#extract sentences in order of appearance
ordered_top_3 = top_3[order_of_appearance, "sentence"]

out <- speech_sentences %>%
  filter(speech_id %in% 1:10) %>% 
  group_by(speech_id) %>%
  mutate(n = n()) %>% 
  ungroup() %>%
  filter(n > 5) %>%
  split(.$speech_id) %>% 
  map(~bind_lexrank(., sentences, speech_id, level="sentences")) 

out %>% 
  reduce(bind_rows) %>%
  group_by(speech_id) %>%
  top_n(1, lexrank)
  arrange(speech_id, desc(lexrank)) %>% head()



# counting statistics -----------------------------------------------------

trigrams <- df %>%
    unnest_tokens(trigram, speeches, token = "ngrams", n = 3)

number_words <- c("ten?s", "dozen?s", "hundred?s", "thousand?s", "million?s", "billion?s")  
  
trigrams %>%
  mutate(stat = ifelse(str_detect(trigram, "[0-9]([^a-z]|$)"), T, F)) %>% filter(stat) %>% View()
