# Initial extraction and munging

# packages ----
pacman::p_load(tidyverse, stringr, forcats, purrr, magrittr, tidytext, reshape2, wordcloud)

# Inputs: Hansard source text, source functions 
eu <- read_file("data/European Union (Notification of Withdrawal) Bill 2017-03-01 (1).txt")
source('scripts\\hansard_src.R')
# Outputs: 
# Appearances_tib: speeches in order with Lord, party, gender etc...
# Full_speeches: concatenated speeches of each lord. 

# extract ----

# Analysis begins with extracting names in order from text.
name_vec <- extract_names(eu)

# Then get parties by looking at text structure and webscraping
appearances_tib <- get_parties(name_vec)

appearances_tib %<>% add_surname()

# Manually fix Lords missing parties ----
appearances_tib %>% filter(is.na(party)) %>% unique()

# Add more variables ----

appearances_tib %<>% add_gender()

# real deal hocus pocus step
appearances_tib_f <- appearances_tib %>% bind_speeches(rm_shouts = T)
appearances_tib %<>% bind_speeches()

full_speeches <- reduce_speeches(appearances_tib)

# Reduce party data
appearances_tib %<>% keep_main_parties()
full_speeches %<>% keep_main_parties()
appearances_tib_f %<>% keep_main_parties()

# save party data
saveRDS(appearances_tib, "data//appearances.RDAT")
saveRDS(appearances_tib_f, "data//appearances_f.RDAT")
saveRDS(full_speeches, "data//full_speeches.RDAT")

# get df of 1:1 mapping speaker-party
party_id_tib <- distinct(full_speeches, name, party)
(party_id_tib %<>% keep_main_parties() %>% add_surname)
saveRDS(party_id_tib, "data//party_id_tib.RDAT")

# Get document-term matrices for informative speeches

tokenised_df <- appearances_tib_f %>% 
  mutate(speech_id = row_number()) %>%
  unnest_tokens(word, speeches) 
  
tidy_dtm <- tokenised_df %>%
  anti_join(stop_words) %>%
  mutate(total_words = length(word)) %>%
  group_by(word) %>% mutate(hansard_total = length(word), hansard_prop = hansard_total/total_words) %>%
  group_by(speech_id) %>% mutate(total_words_speech = length(word)) %>%
  group_by(speech_id, word) %>% mutate(speech_total = length(word), speech_prop = speech_total/total_words_speech) %>%
  distinct() %>% 
  ungroup()

saveRDS(tokenised_df, "data//tokenised_df.RDAT")
saveRDS(tidy_dtm, "data//tidy_dtm.RDAT")
