# 01 March 2017 Hansard European withdrawal bill

pacman::p_load(tidyverse, magrittr, tidytext, stringr, ggpubr)

full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances.RDAT")
party_id_tib <- readRDS("data//party_id_tib.RDAT")
source("scripts/hansard_src.R")

full_speeches$all_speeches %<>% map(~str_replace_all(., "\\\r\\\n\\\r\\\n", " "))
appearances_tib$speeches %<>% map(~str_replace_all(., "\\\r\\\n\\\r\\\n", " "))

# Number of speakers in each party, coloured by gender.
full_speeches %>% 
  group_by(party, gender) %>% 
  summarise(n = n()) %>%
  ggplot(aes(party, n)) +
  geom_col(aes(fill = gender)) +
  ylab("# members") +
  scale_y_continuous(breaks = seq(0,30,5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()

# Number of speeches by party
speeches_by_party <- appearances_tib %>% 
  group_by(party) %>% 
  summarise(n_speeches = n())

# Expected number of speeches per party
exp_speeches <- appearances_tib %>%
  distinct(name, party) %>% 
  group_by(party) %>% 
  summarise(n_distinct_lords = n()) %>%
  mutate(total_speeches = nrow(appearances_tib), nprob = n_distinct_lords/sum(n_distinct_lords), expected_nspeeches = round(total_speeches*nprob)) 

speeches_by_party %>%
  left_join(exp_speeches, by = "party") %>%
  select(party,n_speeches,expected_nspeeches) %>% 
  gather(speech_type, count, n_speeches, expected_nspeeches) %>%
  ggplot(aes(party, count)) + geom_col(aes(fill = speech_type), position = "dodge") +
  scale_y_continuous(name="Number of speeches") + 
  theme_pubr()


# Expected number of speeches for each gender

appearances_tib %>% 
  group_by(gender) %>% 
  summarise(n_speeches = length(name), n_speakers = length(unique(name))) %>%
  ungroup() %>% 
  mutate(tot_s = sum(n_speeches), exp = tot_s * n_speakers/sum(n_speakers)) %>%
  gather(type, count, n_speeches, exp) %>%
  ggplot(aes(gender, count)) + geom_col(aes(fill = type), position = 'dodge') + theme_pubr()


# Expected vs Observed Words  ---------------------------------------------

words_per_speech <- appearances_tib %>% 
  mutate(r = row_number()) %>% 
  unnest_tokens(word, speeches) %>% 
  group_by(r, name) %>% 
  summarise(word_count = n()) %>%
  filter(!is.na(name)) %>% 
  ungroup() %>% 
  add_gender()

words_per_lord <- words_per_speech %>%
  group_by(name) %>% 
  summarise(word_count = sum(word_count)) %>% 
  filter(!is.na(name))

words_per_lord %<>% add_gender()
words_per_lord %<>% mutate(name =  as.factor(name))

words_per_lord %>%
  left_join(party_id_tib) %>% 
  group_by(party) %>% 
  summarise(total_words = sum(word_count), total_speakers = length(name)) %>%
  ungroup() %>%
  mutate(tot_s = sum(total_speakers), tot_w = sum(total_words), exp = tot_w*(total_speakers/tot_s)) %>% 
  gather(type, count, total_words, exp) %>%
  ggplot(aes(party, count)) + geom_col(aes(fill = type), position = 'dodge') +
  scale_fill_manual(labels = c("expected", "actual"), values = c("red", "blue")) +
  scale_y_continuous(name="Number of words") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

appearances_tib %>%
  group_by(name) %>%
  summarise(party = unique(party), n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 3)

words_per_lord %>% 
  group_by(gender) %>%
  summarise(total_words = sum(word_count), total_speakers = length(name)) %>%
  ungroup() %>%
  mutate(tot_s = sum(total_speakers), tot_w = sum(total_words), exp = tot_w*(total_speakers/tot_s)) %>% 
  gather(type, count, total_words, exp) %>%
  ggplot(aes(gender, count)) + geom_col(aes(fill = type), position = 'dodge') + theme_pubr()

# Distribution of word counts per speaker for each gender

words_per_lord %>%
  group_by(gender) %>% 
  arrange(gender, word_count) %>% 
  mutate(r = row_number(), p = r/length(r)) %>% 
  ggplot(aes(word_count, p)) + 
  geom_line(aes(colour = gender)) + 
  geom_point(aes(colour = gender)) + 
  theme_pubr()



# TF-IDF ------------------------------------------------------------------

spkr_words <- appearances_tib %>%
  unnest_tokens(word, speeches) %>%
  count(name, word, sort = T) %>%
  ungroup()

spkr_words %>% head()

total_words <- spkr_words %>%
  group_by(name) %>%
  summarise(total = sum(n))

spkr_words <- left_join(spkr_words, total_words, by = "name")

spkr_words %>%
  group_by(name, total) %>% 
  nest() %>% 
  top_n(6, total) %>%
  unnest() %>%
  ggplot(aes(n/total, fill = name)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~name, ncol = 2, scales = "free_y")

spkr_words <- spkr_words %>%
  bind_tf_idf(word, name, n)
spkr_words  

spkr_words %>%
  filter(total > 200) %>%
  arrange(desc(tf_idf))

top6_spkrs <- spkr_words %>% distinct(name, total) %>% arrange(desc(total)) %>% head(6) %>% pull(name) 

spkr_words %>%
  filter(name %in% top6_spkrs, total > 200) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(name) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~name, ncol = 2, scales = "free") +
  coord_flip()

# Which gender asks what kind of questions?

total_words <- spkr_words %>% distinct(name, total)

q_tib <- appearances_tib %>%
  left_join(total_words) %>%
  filter(total > 200) %>%
  mutate(questions = map(speeches, pull_questions))

q_tib$questions 

q_tib <- q_tib %>%
  mutate(size_qtib = map_int(questions, nrow))

head(q_tib)

q_tib %>% 
  filter(size_qtib > 0) %>%
  select(name, gender, questions, size_qtib) %>%
  unnest() %>% 
  View()

# POS tagging

# https://github.com/bnosac/RDRPOSTagger

library(RDRPOSTagger)

models <- rdr_available_models()
models$MORPH$language
models$POS$language
models$UniversalPOS$language

x <- appearances_tib$speeches[[4]]
tagger <- rdr_model(language = "English", annotation = "POS")
pos <- rdr_pos(tagger, x = x) 


anno <- function(text, pattern = "NN") {
  if (!exists("pos_tag_abbrs")) {
    pos_tag_abbrs <- readRDS("data/pos_tag_abbrs.RDAT")
  }
  print("Note: pattern = .* will return tags for all words in text")
  # Note: pattern = .* will return tags for all words in text
  
  tagger <- rdr_model(language = "English", annotation = "POS")
  pos <- rdr_pos(tagger, x = text) 

  # Make a tibble of the words and tags, filter for the desired part-of-speech (pattern)
  pos %>%
    filter(str_detect(pos, pattern)) %>% left_join(pos_tag_abbrs, by = c("pos" = "abbreviation"))
} 

anno(x, "J")
anno(x, "NN")
anno(x, "V")

select_rows <- function(df, n) {
  df[1:n,]
}

tagged_speeches <- full_speeches %>% 
  select_rows(20) %>%
  mutate(adjs = map(all_speeches, ~anno(., "J") %>% pull(token)))

tagged_speeches %>% 
  select(gender, adjs) %>%
  unnest() %>%
  group_by(gender, adjs) %>%
  count() %>%
  group_by(gender) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(adjs = factor(adjs, levels = rev(unique(adjs)))) %>% 
  ggplot(aes(adjs, n)) + geom_col(aes(fill = gender)) + facet_wrap(~gender, scale = "free")
